{-# LANGUAGE FlexibleContexts #-}

module Lib.Memory
    ( allocateDMA
    , allocateMemPool
    , allocatePktBufBatch
    , allocatePktBuf
    , freePktBuf
    ) where

import Lib.Log (Logger, halt, logLn)
import Lib.Memory.Types (MemPool(..), PacketBuf(..), Translation(..))
import Lib.Prelude

import Control.Monad.Catch (MonadCatch, handleIOError)
import Data.Bits ((.&.), shift, shiftR)
import qualified Data.ByteString as B
import Data.List ((!!))
import Foreign.Ptr (Ptr, castPtr, plusPtr, ptrToWordPtr)
import Foreign.Storable (poke, sizeOf)
import System.IO.Error (userError)
import qualified System.Path as Path
import qualified System.Path.Directory as Dir
import qualified System.Path.IO as PathIO
import System.Posix.IO (closeFd, handleToFd)
import System.Posix.Memory
    ( MemoryMapFlag(MemoryMapShared)
    , MemoryProtection(MemoryProtectionRead, MemoryProtectionWrite)
    , memoryLock
    , memoryMap
    , sysconfPageSize
    )

hugePageBits :: Int
hugePageBits = 21

hugePageSize :: Word
hugePageSize = shift 1 hugePageBits

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

allocateDMA :: (MonadCatch m, MonadIO m, MonadReader env m, Logger env) => Word -> Bool -> m Translation
allocateDMA size contiguous =
    if contiguous && size > hugePageSize
        then halt "Error occured during an attempt to allocate memory for DMA." $ userError "Could not map physically contiguous memory."
        else handleIOError handler inner
  where
    handler = halt "Error occured during an attempt to allocate memory for DMA."
    inner = do
        logLn $ "Attempting to allocate DMA memory with size " <> show size <> "B (contiguous=" <> show contiguous <> ")."
        ptr <-
            liftIO $ do
                (fname, h) <- PathIO.openBinaryTempFile (Path.absDir "/mnt/huge") (Path.relFile "ixy.huge")
                PathIO.hSetFileSize h $ fromIntegral s
                fd <- handleToFd h
                ptr <- memoryMap Nothing (fromIntegral s) [MemoryProtectionRead, MemoryProtectionWrite] MemoryMapShared (Just fd) 0
                memoryLock ptr $ fromIntegral s
                closeFd fd
                Dir.removeFile fname
                return ptr
        phy <- translate ptr
        return Translation {trVirtual = ptr, trPhysical = phy}
    s =
        if size `mod` hugePageSize /= 0
            then shift (shiftR size hugePageBits + 1) hugePageBits
            else size

-- TODO: Currently this whole thing will only work with a bufSize of 2048, because PackeBuf assumes
-- it's always 2048B big. This is a limitation of Storable needing to be fixed size. Only possible
-- fix is something like TemplateHaskell.
allocateMemPool :: (MonadCatch m, MonadIO m, MonadReader env m, Logger env) => Int -> Word -> m MemPool
allocateMemPool numEntries entrySize =
    if hugePageSize `mod` entrySize /= 0
        then halt "Error occured during allocation of a memory pool." $ userError "Entry size must be a divisor of huge page size."
        else do
            logLn $ "Allocating MemPool with numEntries=" <> show numEntries <> " and entrySize=" <> show entrySize <> "."
            t <- allocateDMA (fromIntegral (numEntries * fromIntegral entrySize)) False
    -- NOTE: This ignores entrySize and fixes it to 2048!
            logLn "Entry size is ignored and fixed to 2048B."
            let m =
                    MemPool
                        { mpBase = castPtr (trVirtual t)
                        , mpBufSize = 2048
                        , mpTop = fromIntegral numEntries
                        , mpFreeBufs = [0 .. (numEntries - 1)]
                        }
             in do forM_
                       [ ( mpBase m `plusPtr`
                           fromIntegral
                               (i * fromIntegral (sizeOf (PacketBuf {pbPhysical = 0, pbMemPoolIndex = 0, pbBufSize = 0, pbBuf = B.empty})))
                         , i)
                       | i <- mpFreeBufs m
                       ]
                       setupBuf
                   return m
  where
    setupBuf (ptr, index) = do
        t <- translate ptr
        let packetBuf = PacketBuf {pbPhysical = t, pbMemPoolIndex = fromIntegral index, pbBufSize = 0, pbBuf = B.empty}
         in liftIO $ poke (castPtr ptr) packetBuf

allocatePktBufBatch :: (MonadCatch m, MonadIO m, MonadReader env m, Logger env, MonadState MemPool m) => Int -> m ([Ptr PacketBuf], Int)
allocatePktBufBatch numBufs = do
    memPool <- get
    let n = min (mpTop memPool) numBufs
    ptrs <- forM [0 .. (n - 1)] (getBuf memPool)
    put memPool {mpTop = mpTop memPool - n}
    return (ptrs, n)
    -- NOTE: Now that I think about it, is MemPool a Monoid?
  where
    getBuf memPool index =
        let entryId = mpFreeBufs memPool !! (mpTop memPool - index)
         in return $
            mpBase memPool `plusPtr` (entryId * sizeOf (PacketBuf {pbPhysical = 0, pbMemPoolIndex = 0, pbBufSize = 0, pbBuf = B.empty}))

allocatePktBuf :: (MonadCatch m, MonadIO m, MonadReader env m, Logger env, MonadState MemPool m) => m (Ptr PacketBuf, Int)
allocatePktBuf = do
    ([buf], n) <- allocatePktBufBatch 1
    return (buf, n)

freePktBuf :: (MonadIO m, MonadState MemPool m) => PacketBuf -> m ()
freePktBuf packetBuf = do
    memPool <- get
    let (x, _:ys) = splitAt (mpTop memPool) (mpFreeBufs memPool)
     in put memPool {mpTop = mpTop memPool + 1, mpFreeBufs = x ++ pbMemPoolIndex packetBuf : ys}
