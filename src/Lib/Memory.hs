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
  , translate
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
import           Foreign.Ptr                    ( WordPtr(..)
                                                , ptrToWordPtr
                                                )
import           System.IO.Error                ( userError )
import           System.IO.Unsafe               ( unsafePerformIO )
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
