module Lib.Pci
    ( enableDMA
    , unbind
    , mapResource
    ) where

import Lib.Ixgbe.Types (Device(..), DeviceState)
import Lib.Log (Logger, halt, logLn)
import Lib.Pci.Types (BusDeviceFunction(unBusDeviceFunction))
import Lib.Prelude hiding (writeFile)

import Control.Monad.Catch (MonadCatch, handleIOError)
import Data.Bits ((.|.), shift)
import qualified Data.ByteString as B
import qualified Data.Text as Text
import System.IO.Error (isDoesNotExistError)
import qualified System.Path as Path
import System.Path ((</>))
import qualified System.Path.IO as PathIO
import System.Posix.IO (handleToFd)
import System.Posix.Memory (MemoryMapFlag(MemoryMapShared), MemoryProtection(MemoryProtectionRead, MemoryProtectionWrite), memoryMap)

base :: Path.AbsDir
base = Path.dirPath "/sys/bus/pci/devices/"

enableDMA :: (MonadCatch m, MonadIO m, MonadReader env m, Logger env, DeviceState m) => m ()
enableDMA = do
    dev <- get
    let bdf = unBusDeviceFunction $ devBdf dev
    let path = base </> Path.relPath (Text.unpack bdf) </> Path.filePath "config"
    handleIOError
        handler
        (do logLn $ "Enabling DMA for PCI device " <> bdf <> "."
            liftIO $
                PathIO.withBinaryFile
                    path
                    PathIO.ReadWriteMode
                    (\h -> do
                         PathIO.hSeek h PathIO.AbsoluteSeek cmdRegOffset
                         value <- B.hGet h 2
                         PathIO.hSeek h PathIO.AbsoluteSeek cmdRegOffset
                         B.hPut h $ setDMA value))
  where
    cmdRegOffset = 4
    busMasterEnableIndex = 2
    setDMA b = (B.head b .|. shift 1 busMasterEnableIndex) `B.cons` B.tail b
    handler = halt "Error occured during an attempt to enable DMA"

mapResource :: (MonadCatch m, MonadIO m, MonadReader env m, Logger env, DeviceState m) => Text -> m (Ptr a)
mapResource resource = do
    dev <- get
    let bdf = unBusDeviceFunction $ devBdf dev
    let res = Path.relFile $ Text.unpack resource
    let path = base </> Path.relPath (Text.unpack bdf) </> res
    handleIOError
        handler
        (do logLn $ "Mapping resource \'" <> resource <> "\' for PCI device " <> bdf <> "."
            unbind
            enableDMA
            liftIO $
                PathIO.withBinaryFile
                    path
                    PathIO.ReadWriteMode
                    (\h ->
                         liftIO $ do
                             size <- PathIO.hFileSize h
                             fd <- handleToFd h
                             memoryMap Nothing (fromIntegral size) [MemoryProtectionRead, MemoryProtectionWrite] MemoryMapShared (Just fd) 0))
  where
    handler = halt "Error occured during the mapping of a PCI resource"

unbind :: (MonadCatch m, MonadIO m, MonadReader env m, Logger env, DeviceState m) => m ()
unbind = do
    dev <- get
    let bdf = unBusDeviceFunction $ devBdf dev
    let path = base </> Path.relPath (Text.unpack bdf) </> Path.filePath "driver/unbind"
    handleIOError
        (handler bdf)
        (do logLn $ "Attempting to unbind driver for PCI device " <> bdf <> "."
            liftIO $ PathIO.writeFile path $ Text.unpack bdf)
  where
    handler _ e
        | isDoesNotExistError e = logLn "Driver was not unbound, because \'unbind\' file was missing. (Already unbound?)"
    handler bdf e = halt ("Error occured during the unbind operation for PCI device " <> bdf <> ".") e
