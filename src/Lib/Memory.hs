{-# LANGUAGE TemplateHaskell #-}
-- |
-- Module      :  Lib.Memory
-- Copyright   :  Alex Egger 2018
-- License     :  BSD3
--
-- Maintainer  :  alex.egger96@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
--
--
module Lib.Memory
  ( allocateMem
  , mkMemPool
  , allocateBuf
  , readBuf
  , getBufferPtr
  , nrOfFreeBufs
  , freeBuf
  , translate
  , addrOffset
  , sizeOffset
  , dataOffset
  , MemPool(..)
  , PacketBuf(..)
  , PhysAddr(..)
  , VirtAddr(..)
  )
where

import           Lib.Prelude

import           Control.Monad.Logger           ( MonadLogger
                                                , logDebug
                                                )
import           Control.Monad.Catch     hiding ( bracket )
import           Data.Binary.Get
import qualified Data.ByteString               as B
import           Data.ByteString.Unsafe
import           Data.IORef
import           Foreign.Marshal.Utils          ( copyBytes )
import           Foreign.Ptr                    ( WordPtr(..)
                                                , castPtr
                                                , plusPtr
                                                , ptrToWordPtr
                                                )
import           Foreign.Storable               ( sizeOf
                                                , alignment
                                                , peek
                                                , peekByteOff
                                                , poke
                                                , pokeByteOff
                                                )
import           System.IO.Error                ( userError )
import qualified System.Path                   as Path
import qualified System.Path.IO                as PathIO
import           System.Posix.IO                ( closeFd
                                                , handleToFd
                                                )
import           System.Posix.Memory            ( MemoryMapFlag(MemoryMapShared)
                                                , MemoryProtection
                                                  ( MemoryProtectionRead
                                                  , MemoryProtectionWrite
                                                  )
                                                , memoryLock
                                                , memoryMap
                                                , sysconfPageSize
                                                )

newtype PhysAddr = PhysAddr Word64
newtype VirtAddr a = VirtAddr (Ptr a)

-- $ Huge Pages

hugepageBits :: Int
hugepageBits = 21

hugepageSize :: Int
hugepageSize = shift 1 hugepageBits

-- $ Allocations

allocateMem
  :: (MonadThrow m, MonadIO m, MonadLogger m) => Int -> Bool -> m (Ptr a)
allocateMem size contiguous = do
  $(logDebug)
    $  "Allocating a memory chunk with size "
    <> show size
    <> "B (contiguous="
    <> show contiguous
    <> ")."
  let s = if size `mod` hugepageSize /= 0
        then shift (shiftR size hugepageBits + 1) hugepageBits
        else size
  liftIO $ do
    (_, h) <- PathIO.openBinaryTempFile (Path.absDir "/mnt/huge")
                                        (Path.relFile "ixy.huge")
    PathIO.hSetFileSize h $ fromIntegral s
    let f = memoryMap Nothing
                      (fromIntegral s)
                      [MemoryProtectionRead, MemoryProtectionWrite]
                      MemoryMapShared
    ptr <- bracket (handleToFd h) closeFd (\fd -> Just fd `f` 0)
    memoryLock ptr $ fromIntegral s
    -- TODO: We should remove this, but not here.
    -- Dir.removeFile fname
    return ptr

-- $ Memory Pools
data PacketBuf = PacketBuf { pbId :: Int
                           , pbAddr :: PhysAddr
                           , pbSize :: Int
                           , pbData :: ByteString }

instance Storable PacketBuf where
  sizeOf _ = 2048
  alignment = sizeOf
  peek ptr = do
    id <- peek (castPtr ptr)
    addr <- peekByteOff ptr addrOffset
    size <- peekByteOff ptr sizeOffset
    bufData <- unsafePackCStringLen (castPtr (ptr `plusPtr` dataOffset), size)
    return PacketBuf {pbId =id , pbAddr = PhysAddr addr, pbSize = size, pbData = bufData}
  poke ptr buf = do
    poke (castPtr ptr) $ pbId buf
    pokeByteOff ptr addrOffset bufAddr
    pokeByteOff ptr sizeOffset $ pbSize buf
    unsafeUseAsCStringLen (pbData buf) $ uncurry (copyBytes (ptr `plusPtr` dataOffset))
   where PhysAddr bufAddr = pbAddr buf

addrOffset :: Int
addrOffset = sizeOf (0 :: Int)

sizeOffset :: Int
sizeOffset = addrOffset + sizeOf (0 :: Word64)

dataOffset :: Int
dataOffset = sizeOffset + sizeOf (0 :: Int)

data MemPool = MemPool { mpBaseAddr :: Ptr Word8
                       , mpNumEntries :: Int
                       , mpFreeBufs :: IORef [Int]
                       }

mkMemPool :: (MonadThrow m, MonadIO m, MonadLogger m) => Int -> m MemPool
mkMemPool numEntries = do
  ptr <- allocateMem (numEntries * bufSize) False
  mapM_ initBuf
        [ (ptr `plusPtr` (i * bufSize), i) | i <- [0 .. numEntries - 1] ]
  freeBufsRef <- liftIO $ newIORef [0 .. numEntries - 1]
  return MemPool
    { mpBaseAddr   = ptr
    , mpNumEntries = numEntries
    , mpFreeBufs   = freeBufsRef
    }
 where
  initBuf (bufPtr, i) = do
    -- TODO: BufPhysAddr points at data, is this correct?
    bufPhysAddr <- liftIO $ translate $ VirtAddr (bufPtr `plusPtr` dataOffset)
    liftIO $ poke
      bufPtr
      PacketBuf {pbId = i, pbAddr = bufPhysAddr, pbSize = 0, pbData = B.empty}
  bufSize = sizeOf (undefined :: PacketBuf)

allocateBuf :: MemPool -> IO (Ptr PacketBuf)
allocateBuf memPool = do
  freeBufs <- readIORef $ mpFreeBufs memPool
  case freeBufs of
    (x : xs) -> do
      writeIORef (mpFreeBufs memPool) xs
      return
        $         mpBaseAddr memPool
        `plusPtr` (x * sizeOf (undefined :: PacketBuf))
    [] -> panic "MemPool is empty."

nrOfFreeBufs :: MemPool -> IO Int
nrOfFreeBufs memPool = length <$> liftIO (readIORef (mpFreeBufs memPool))

getBufferPtr :: MemPool -> Int -> Ptr PacketBuf
getBufferPtr memPool id =
  mpBaseAddr memPool `plusPtr` (id * sizeOf (undefined :: PacketBuf))

readBuf :: MemPool -> Int -> IO PacketBuf
readBuf memPool id = peek $ getBufferPtr memPool id

freeBuf :: MemPool -> Int -> IO ()
freeBuf memPool id = modifyIORef (mpFreeBufs memPool) (id :)

-- $ Utility

translate :: VirtAddr a -> IO PhysAddr
translate (VirtAddr virt) = PathIO.withBinaryFile path PathIO.ReadMode inner
 where
  inner h = do
    PathIO.hSeek h PathIO.AbsoluteSeek $ fromIntegral offset
    buf <- B.hGet h 8
    case runGetIncremental getWord64le `pushChunk` buf of
      Done _ _ b -> return $ PhysAddr $ getAddr $ fromIntegral b
      Partial _ ->
        throwM $ userError "Partial input when parsing physical address."
      Fail{} -> throwM $ userError "Physical address was malformed."
  path         = Path.absFile "/proc/self/pagemap"
  WordPtr addr = ptrToWordPtr virt
  offset       = (addr `quot` pageSize) * 8 -- This is not arch-specific, hence the magic number.
  getAddr x =
    fromIntegral $ (x .&. 0x7fffffffffffff) * pageSize + addr `mod` pageSize
  pageSize = fromIntegral sysconfPageSize
