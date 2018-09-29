{-# LANGUAGE FlexibleContexts #-}

module Lib.Memory
    ( allocateDMA
    , allocateMemPool
    , allocatePktBufBatch
    , allocatePktBuf
    ) where

import Lib.Log (Logger, halt, logLn)
import Lib.Memory.Types (MemPool(..), PacketBuf(..), Translation(..))
import Lib.Prelude

import Control.Monad.Catch (MonadCatch, handleIOError)
import Data.Bits ((.&.), shift, shiftR)
import qualified Data.ByteString as B
import Data.Foldable (mapM_)
import Foreign.Ptr (Ptr, castPtr, nullPtr, plusPtr, ptrToWordPtr)
import Foreign.Storable (peekByteOff, pokeByteOff, sizeOf)
import qualified System.Path as Path
import qualified System.Path.IO as PathIO
import System.Posix.IO (closeFd, handleToFd)
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

allocateDMA :: (MonadCatch m, MonadIO m, MonadReader env m, Logger env) => Word -> Bool -> m Translation
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
                ptr <- memoryMap Nothing (fromIntegral s) [MemoryProtectionRead, MemoryProtectionWrite] MemoryMapShared (Just fd) 0
                closeFd fd
                return ptr
        phy <- translate ptr
        return Translation {trVirtual = ptr, trPhysical = phy}
    s =
        if size `mod` hugePageSize /= 0
            then shift (shiftR size hugePageBits + 1) hugePageBits
            else size

translate :: (MonadCatch m, MonadIO m, MonadReader env m, Logger env) => Ptr Word -> m Word
translate virt = handleIOError handler (liftIO $ PathIO.withBinaryFile path PathIO.ReadMode inner)
  where
    path = Path.absFile "/proc/self/pagemap"
    offset = (fromIntegral wordPtr `quot` sysconfPageSize) * sizeOf wordPtr
    wordPtr = ptrToWordPtr virt
    inner h = do
        PathIO.hSeek h PathIO.AbsoluteSeek $ fromIntegral offset
        fmap (calculateAddress . bsToWord) (B.hGet h $ sizeOf wordPtr)
      where
        bsToWord = B.foldl' (\x y -> x * 256 + fromIntegral y) 0
        calculateAddress addr =
            (addr .&. 0x7fffffffffffff) * fromIntegral sysconfPageSize + fromIntegral wordPtr `mod` fromIntegral sysconfPageSize
    handler = halt "Error occured during translation of a virtual address."

-- TODO: Currently this whole thing will only work with a bufSize of 2048, because PackeBuf assumes
-- it's always 2048B big. This is a limitation of Storable needing to be fixed size. Only possible
-- fix is something like compile-time size calculation.
allocateMemPool :: (MonadCatch m, MonadIO m, MonadReader env m, Logger env) => Word -> Word -> m MemPool
allocateMemPool numEntries entrySize = do
    logLn $ "Allocating MemPool with numEntries=" <> show numEntries <> " and entrySize=" <> show entrySize <> "."
    t <- allocateDMA (fromIntegral (numEntries * entrySize)) False
    -- NOTE: This ignores entrySize and fixes it to 2048!
    logLn "Entry size is ignored and fixed to 2048B."
    let m = MemPool {mpBase = trVirtual t, mpBufSize = 2048, mpTop = fromIntegral numEntries}
     in do mapM_ (initBuf m) [0 .. (numEntries - 1)]
           return m
  where
    initBuf memPool index = do
        packetBuf <- liftIO $ peekByteOff ptr offset
        t <- translate (ptr `plusPtr` offset)
        let pb = packetBuf {pbPhysical = t, pbMemPoolIndex = fromIntegral index, pbBufSize = 0}
         in liftIO $ pokeByteOff ptr offset pb
      where
        ptr = castPtr $ mpBase memPool :: Ptr PacketBuf
        offset = fromIntegral (index * mpBufSize memPool)

-- TODO: Add exception handling to this.
allocatePktBufBatch :: (MonadCatch m, MonadIO m, MonadReader env m, Logger env, MonadState MemPool m) => Int -> m ([Ptr PacketBuf], Int)
allocatePktBufBatch numBufs = do
    logLn $ "Allocating a batch of packet buffers (numBufs=" <> show numBufs <> ")."
    memPool <- get
    let n = min (mpTop memPool) numBufs
     in do bufs <- traverse initBuf [0 .. numBufs]
           return (bufs, n)
  where
    initBuf _ = do
        memPool <- get
        put (memPool {mpTop = mpTop memPool - 1})
        return $
            castPtr
                (nullPtr `plusPtr` (fromIntegral (ptrToWordPtr $ mpBase memPool) + (mpTop memPool - 1) * fromIntegral (mpBufSize memPool)))

allocatePktBuf :: (MonadCatch m, MonadIO m, MonadReader env m, Logger env, MonadState MemPool m) => m (Ptr PacketBuf, Int)
allocatePktBuf = do
    ([buf], n) <- allocatePktBufBatch 1
    return (buf, n)
