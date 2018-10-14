{-# LANGUAGE TemplateHaskell #-}
-- |
-- Module      :  Lib.Pci
-- Copyright   :  Alex Egger 2018
-- License     :  BSD3
--
-- Maintainer  :
-- Stability   :  experimental
-- Portability :  unknown
--
-- Provides utility function to work with PCI devices using sysfs.
module Lib.Pci
  ( -- * BusDeviceFunction
    BusDeviceFunction(unBusDeviceFunction)
  , busDeviceFunction
    -- * Operations
  , mapResource
  )
where

import           Lib.Prelude

import           Control.Monad.Catch
import           Data.Bits                      ( shift
                                                , (.|.)
                                                )
import qualified Data.ByteString               as B
import qualified Data.Text                     as T
import           System.IO.Error                ( isDoesNotExistError )
import           System.Path                    ( (</>) )
import qualified System.Path                   as Path
import qualified System.Path.IO                as PathIO
import           System.Posix.IO                ( handleToFd )
import           Control.Monad.Logger           ( MonadLogger
                                                , logDebug
                                                )
import           System.Posix.Memory            ( MemoryMapFlag(MemoryMapShared)
                                                , MemoryProtection
                                                  ( MemoryProtectionRead
                                                  , MemoryProtectionWrite
                                                  )
                                                , memoryMap
                                                )
import           Text.Regex.PCRE                ( (=~) )

-- | A set of Bus, Device, and Function identifiers that identify a PCI device.
--
-- The identifier follows the extended BDF form: XXXX:BB:DD.F
--
-- where X = PCI Domain Number
--
--       B = PCI Bus Number
--
--       D = PCI Device Number
--
--       F = PCI Function Number
newtype BusDeviceFunction = BDF
  { unBusDeviceFunction :: Text
  } deriving (Show)

-- | Creates a 'BusDeviceFunction' when supplied with an appropiately formed 'Text', or return
-- 'Nothing' if the argument was malformed.
--
-- To see the formatting requirements see 'BusDeviceFunction'.
busDeviceFunction :: Text -> Maybe BusDeviceFunction
busDeviceFunction bdfText
  | T.unpack bdfText =~ T.unpack "[0-9A-F]{4}:[0-9A-F]{2}:[0-9A-F]{2}.[0-9]"
  = Just (BDF bdfText)
busDeviceFunction _ = Nothing

base :: Path.AbsDir
base = Path.absDir "/sys/bus/pci/devices/"

-- $ Operations

-- | Map a resource of a PCI device into memory.
mapResource
  :: (MonadCatch m, MonadThrow m, MonadIO m, MonadLogger m)
  => BusDeviceFunction -- ^ The 'BusDeviceFunction' of the device the resource that will be mapped belongs to.
  -> Text -- ^ The filename of the resource that will be mapped.
  -> m (Ptr a) -- ^ A 'Ptr' to the beginning of the mapped resource.
mapResource bdf resource =
  let path =
        base
          </> Path.relPath (T.unpack $ unBusDeviceFunction bdf)
          </> Path.relFile (T.unpack resource)
  in  do
        $(logDebug)
          $  "Mapping resource \'"
          <> resource
          <> "\' for device "
          <> unBusDeviceFunction bdf
          <> "."
        unbind bdf
        enableDMA bdf
        liftIO $ PathIO.withBinaryFile path PathIO.ReadWriteMode inner
 where
  inner h = do
    size <- PathIO.hFileSize h
    fd   <- handleToFd h
    memoryMap Nothing
              (fromIntegral size)
              [MemoryProtectionRead, MemoryProtectionWrite]
              MemoryMapShared
              (Just fd)
              0

-- $Internal
-- | Enable DMA for a PCI device.
enableDMA
  :: (MonadIO m, MonadLogger m)
  => BusDeviceFunction -- ^ The 'BusDeviceFunction' of the device for which DMA will be enabled.
  -> m ()
enableDMA bdf =
  let path =
        base
          </> Path.relPath (T.unpack $ unBusDeviceFunction bdf)
          </> Path.relFile "config"
  in  inner path
 where
  inner path = do
    $(logDebug) $ "Enabling DMA for device " <> unBusDeviceFunction bdf <> "."
    liftIO $ PathIO.withBinaryFile
      path
      PathIO.ReadWriteMode
      (\h -> do
        PathIO.hSeek h PathIO.AbsoluteSeek cmdRegOffset
        value <- B.hGet h 2
        PathIO.hSeek h PathIO.AbsoluteSeek cmdRegOffset
        B.hPut h $ setDMA value
      )
  cmdRegOffset         = 4
  busMasterEnableIndex = 2
  setDMA b = (B.head b .|. shift 1 busMasterEnableIndex) `B.cons` B.tail b

-- | Unbind a PCI device from its driver.
unbind
  :: (MonadCatch m, MonadThrow m, MonadIO m, MonadLogger m)
  => BusDeviceFunction -- ^ The 'BusDeviceFunction' of the device that will be unbound.
  -> m ()
unbind bdf =
  let path =
        base
          </> Path.relPath (T.unpack $ unBusDeviceFunction bdf)
          </> Path.filePath "driver/unbind"
  in  catchIOError (inner path) handler
 where
  inner path = do
    $(logDebug)
      $  "Unbinding driver for device "
      <> unBusDeviceFunction bdf
      <> "."
    liftIO $ PathIO.writeFile path $ T.unpack $ unBusDeviceFunction bdf
  handler e | isDoesNotExistError e = $(logDebug) "Device was already unbound."
  handler e                         = throwM e
