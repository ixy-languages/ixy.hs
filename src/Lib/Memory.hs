{-# LANGUAGE FlexibleContexts #-}

module Lib.Memory
    ( translate
    , allocateDMA
    , allocateMemPool
    , MemPool(MemPool)
    , base
    , bufSize
    , top
    ) where

import Lib.Log (Logger, halt, logLn)
import Lib.Prelude

import Control.Monad.Catch (MonadCatch, handleIOError)
import Data.Bits ((.&.), shift, shiftR)
import qualified Data.ByteString as B
import Data.Foldable (mapM_)
import Foreign.Marshal.Alloc (free, malloc)
import Foreign.Ptr (Ptr, castPtr, nullPtr, plusPtr, ptrToWordPtr)
import Foreign.Storable (alignment, peek, peekByteOff, poke, pokeByteOff, sizeOf)
import qualified System.Path as Path
import qualified System.Path.IO as PathIO
import System.Posix.IO (handleToFd)
import System.Posix.Memory
    ( MemoryMapFlag(MemoryMapShared)
    , MemoryProtection(MemoryProtectionRead, MemoryProtectionWrite)
    , memoryMap
    , sysconfPageSize
    )

hugePageBits :: Int
hugePageBits = 21

hugePageSize :: Word
hugePageSize = shift 1 hugePageBits

translate :: (MonadCatch m, MonadIO m, Logger m) => Ptr a -> m Word
translate virt = handleIOError handler (liftIO $ PathIO.withBinaryFile path PathIO.ReadWriteMode inner)
  where
    path = Path.absFile "/proc/self/pagemap"
    offset = fromIntegral $ fromIntegral wPtr `quot` sysconfPageSize * sizeOf wPtr :: Integer
    wPtr = ptrToWordPtr virt
    inner h = do
        PathIO.hSeek h PathIO.AbsoluteSeek offset
        b <- malloc :: IO (Ptr Word)
        _ <- PathIO.hGetBuf h b $ sizeOf wPtr
        phy <- peek b
        free b
        return $ (phy .&. 0x7fffffffffffff) * fromIntegral sysconfPageSize + fromIntegral wPtr `mod` fromIntegral sysconfPageSize
    handler = halt "Error occured during translation of a virtual address."

data Translation = Translation
    { physical :: Word
    , virtual :: Ptr Word
    }

allocateDMA :: (MonadCatch m, MonadIO m, Logger m) => Word -> Bool -> m Translation
allocateDMA size contiguous = handleIOError handler inner
  where
    handler = halt "Error occured during an attempt to allocate memory for DMA."
    inner = do
        logLn $ "Attempting to allocate DMA memory with size " <> show size <> "B (contiguous=" <> show contiguous <> ")."
        ptr <-
            liftIO $ do
                (_, h) <- PathIO.openBinaryTempFile (Path.absDir "/mnt/huge") (Path.relFile "ixy.huge")
                PathIO.hSetFileSize h $ fromIntegral s
                fd <- handleToFd h
                memoryMap Nothing (fromIntegral s) [MemoryProtectionRead, MemoryProtectionWrite] MemoryMapShared (Just fd) 0
        phy <- translate ptr
        return Translation {virtual = ptr, physical = phy}
    s =
        if size `mod` hugePageSize /= 0
            then shift (shiftR size hugePageBits + 1) hugePageBits
            else size

-- TODO: Currently this whole thing will only work with a bufSize of 2048, because PackeBuf assumes
-- it's always 2048B big. This is a limitation of Storable needing to be fixed size. Only possible
-- fix is something like compile-time size calculation.
data MemPool = MemPool
    { base :: Ptr Word
    , bufSize :: Int
    , top :: Int
    }

allocateMemPool :: (MonadCatch m, MonadIO m, Logger m) => Int -> Int -> m MemPool
allocateMemPool numEntries entrySize = do
    logLn $ "Allocating MemPool with numEntries=" <> show numEntries <> " and entrySize=" <> show entrySize <> "."
    t <- allocateDMA (fromIntegral (numEntries * entrySize)) False
    -- NOTE: This ignores entrySize and fixes it to 2048!
    logLn "Entry size is ignored and fixed to 2048B."
    let m = MemPool {base = virtual t, bufSize = 2048, top = numEntries}
     in do mapM_ (initBuf m) [0 .. (numEntries - 1)]
           return m
  where
    initBuf memPool index = do
        packetBuf <- liftIO $ peekByteOff ptr offset
        t <- translate (ptr `plusPtr` offset)
        let pb = packetBuf {physicalAddr = t, memPoolIndex = index, size = 0}
         in liftIO $ pokeByteOff ptr offset pb
      where
        ptr = castPtr $ base memPool :: Ptr PacketBuf
        offset = index * bufSize memPool

data PacketBuf = PacketBuf
    { physicalAddr :: Word
    , memPoolIndex :: Int
    , size :: Int
    , buf :: ByteString
    } deriving (Show)

instance Storable PacketBuf where
    sizeOf _ = 2048
    alignment _ = 2048
    peek ptr = do
        addr <- peek (castPtr ptr) :: IO Word
        index <- peekByteOff ptr (sizeOf addr) :: IO Int
        size <- peekByteOff ptr (sizeOf addr + sizeOf index) :: IO Int
        let indices =
                if size /= 0
                    then [0 .. size]
                    else []
        bufRaw <- mapM (peekWord ptr (sizeOf addr + sizeOf index + sizeOf size)) indices
        return PacketBuf {physicalAddr = addr, memPoolIndex = index, size = size, buf = B.pack bufRaw}
      where
        peekWord ptr offset i = peekByteOff ptr (offset + i * sizeOf (undefined :: Word8)) :: IO Word8
    poke ptr memPool = do
        poke (castPtr ptr :: Ptr Word) phy
        pokeByteOff ptr (sizeOf phy) index
        pokeByteOff ptr (sizeOf phy + sizeOf index) s
        mapM_ (pokeWord ptr (sizeOf phy + sizeOf index + sizeOf s)) $ zip [0 .. s] b
      where
        phy = physicalAddr memPool
        index = memPoolIndex memPool
        s = size memPool
        b = B.unpack $ buf memPool
        pokeWord ptr offset (i, w) = pokeByteOff ptr (offset + i * sizeOf w) w

-- TODO: Add exception handling to this.
allocateBatch :: (MonadCatch m, MonadIO m, Logger m, MonadState MemPool m) => Int -> m ([Ptr PacketBuf], Int)
allocateBatch numBufs = do
    logLn $ "Allocating a batch of packet buffers (numBufs=" <> show numBufs <> ")."
    memPool <- get
    let n = min (top memPool) numBufs
     in do bufs <- traverse initBuf [0 .. numBufs]
           return (bufs, n)
  where
    initBuf _ = do
        memPool <- get
        put (MemPool {base = base memPool, bufSize = bufSize memPool, top = top memPool - 1})
        return $ castPtr (nullPtr `plusPtr` (fromIntegral (ptrToWordPtr $ base memPool) + (top memPool - 1) * bufSize memPool))
