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
  , idToPtr
  , freeBuf
  , translate
  , peekId
  , peekAddr
  , peekSize
  , pokeSize
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
import qualified Data.Array.IO                 as Array
import qualified Data.Array.Base               as Array
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
    return PacketBuf {pbId = id, pbAddr = PhysAddr addr, pbSize = size, pbData = bufData}
  poke ptr buf = do
    poke (castPtr ptr) $ pbId buf
    pokeByteOff ptr addrOffset bufAddr
    pokeByteOff ptr sizeOffset $ pbSize buf
    unsafeUseAsCStringLen (pbData buf) $ uncurry (copyBytes (ptr `plusPtr` dataOffset))
   where PhysAddr bufAddr = pbAddr buf

addrOffset :: Int
addrOffset = sizeOf (0 :: Int)
{-# INLINE addrOffset #-}

sizeOffset :: Int
sizeOffset = addrOffset + sizeOf (0 :: Word64)
{-# INLINE sizeOffset #-}

dataOffset :: Int
dataOffset = sizeOffset + sizeOf (0 :: Int)
{-# INLINE dataOffset #-}

data MemPool = MemPool { mpBaseAddr :: Ptr Word8
                       , mpNumEntries :: Int
                       , mpFreeBufs :: Array.IOUArray Int Int
                       , mpTop :: IORef Int
                       }

mkMemPool :: (MonadThrow m, MonadIO m, MonadLogger m) => Int -> m MemPool
mkMemPool numEntries = do
  ptr <- allocateMem (numEntries * bufSize) False
  mapM_ initBuf
        [ (ptr `plusPtr` (i * bufSize), i) | i <- [0 .. numEntries - 1] ]
  freeBufs <- liftIO
    $ Array.newListArray (0, numEntries - 1) [0 .. numEntries - 1]
  topRef <- liftIO $ newIORef (numEntries :: Int)
  return MemPool
    { mpBaseAddr   = ptr
    , mpNumEntries = numEntries
    , mpFreeBufs   = freeBufs
    , mpTop        = topRef
    }
 where
  initBuf (bufPtr, i) = do
    bufPhysAddr <- liftIO $ translate $ VirtAddr (bufPtr `plusPtr` dataOffset)
    liftIO $ poke
      bufPtr
      PacketBuf {pbId = i, pbAddr = bufPhysAddr, pbSize = 0, pbData = B.empty}
  bufSize = sizeOf (undefined :: PacketBuf)

-- TODO: prove that unsafeRead always succeeds
allocateBuf :: MemPool -> IO (Ptr PacketBuf)
allocateBuf memPool = do
  top <- subtract 1 <$> readIORef topRef
  writeIORef topRef $! top
  id  <- Array.unsafeRead (mpFreeBufs memPool) top
  return $ idToPtr memPool id
  where
    topRef = mpTop memPool
{-# INLINE allocateBuf #-}

idToPtr :: MemPool -> Int -> Ptr PacketBuf
idToPtr memPool id =
  (mpBaseAddr memPool) `plusPtr` (id * sizeOf (undefined :: PacketBuf))
{-# INLINE idToPtr #-}

peekId :: Ptr PacketBuf -> IO Int
peekId ptr = peek (castPtr ptr)
{-# INLINE peekId #-}

peekAddr :: Ptr PacketBuf -> IO PhysAddr
peekAddr ptr = PhysAddr <$> peekByteOff ptr addrOffset
{-# INLINE peekAddr #-}

peekSize :: Ptr PacketBuf -> IO Int
peekSize ptr = peekByteOff ptr sizeOffset
{-# INLINE peekSize #-}

pokeSize :: Ptr PacketBuf -> Int -> IO ()
pokeSize ptr = pokeByteOff ptr sizeOffset
{-# INLINE pokeSize #-}

-- TODO: prove that unsafeWrite always succeeds
freeBuf :: MemPool -> Int -> IO ()
freeBuf memPool id = do
  let topRef = mpTop memPool
  top <- readIORef topRef
  Array.unsafeWrite (mpFreeBufs memPool) top id
  writeIORef topRef $! top + 1
{-# INLINE freeBuf #-}

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
