module Lib.Pci
    (
    ) where

import Lib.Prelude hiding (writeFile)

import Control.Monad.Catch (MonadCatch, handleIf)
import Data.Bits ((.|.), shift)
import qualified Data.ByteString as B
import Data.Text as Text
import System.IO.Error (isDoesNotExistError)
import System.IO.MMap (Mode(ReadWrite), mmapFilePtr)
import qualified System.Path as Path
import System.Path ((</>))
import qualified System.Path.Directory as Dir
import qualified System.Path.IO as PathIO

newtype BusDeviceFunction = BDF
    { unBusDeviceFunction :: Text
    }

base :: Path.AbsDir
base = Path.dirPath "/sys/bus/pci/devices/"

enableDMA :: (MonadIO m) => BusDeviceFunction -> m ()
enableDMA bdf =
    liftIO $
    PathIO.withBinaryFile
        path
        PathIO.ReadWriteMode
        (\h -> do
             PathIO.hSeek h PathIO.AbsoluteSeek cmdRegOffset
             value <- B.hGet h 2
             B.hPut h $ setDMA value)
  where
    path = base </> Path.relPath (unpack bdfT) </> Path.filePath "config"
    bdfT = unBusDeviceFunction bdf
    cmdRegOffset = 4
    busMasterEnableIndex = 2
    setDMA b = (B.head b .|. shift 1 busMasterEnableIndex) `B.cons` B.tail b

mapResource :: (MonadIO m) => BusDeviceFunction -> Path.RelFile -> m (Ptr a)
mapResource bdf resource =
    liftIO $ do
        unbind bdf
        enableDMA bdf
        (ptr, _, _, _) <- mmapFilePtr (Path.toString path) ReadWrite Nothing
        return ptr
  where
    path = base </> Path.relPath (unpack bdfT) </> resource
    bdfT = unBusDeviceFunction bdf

unbind :: (MonadCatch m, MonadIO m) => BusDeviceFunction -> m ()
unbind bdf = handleIf isDoesNotExistError handler (liftIO $ PathIO.writeFile path $ unpack bdfT)
  where
    path = base </> Path.relPath (unpack bdfT) </> Path.filePath "driver/unbind"
    bdfT = unBusDeviceFunction bdf
    handler _ = return ()
