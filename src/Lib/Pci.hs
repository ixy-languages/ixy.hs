module Lib.Pci
    ( BusDeviceFunction
    , busDeviceFunction
    , enableDMA
    , unbind
    , mapResource
    ) where

import Lib.Prelude hiding (writeFile)

import Control.Monad.Catch (MonadCatch, handleIf)
import Data.Bits ((.|.), shift)
import qualified Data.ByteString as B
import qualified Data.Text as Text
import System.IO.Error (isDoesNotExistError)
import qualified System.Path as Path
import System.Path ((</>))
import qualified System.Path.IO as PathIO
import System.Posix.IO (handleToFd)
import System.Posix.Memory
    ( MemoryMapFlag(MemoryMapShared)
    , MemoryProtection(MemoryProtectionRead, MemoryProtectionWrite)
    , memoryMap
    )
import Text.Regex.PCRE ((=~))

newtype BusDeviceFunction = BDF
    { unBusDeviceFunction :: Text
    }

busDeviceFunction :: Text -> Maybe BusDeviceFunction
busDeviceFunction bdfText
    | Text.unpack bdfText =~ Text.unpack "[0-9A-F]{4}:[0-9A-F]{2}:[0-9A-F]{2}.[0-9]" = Just (BDF bdfText)
busDeviceFunction _ = Nothing

base :: Path.AbsDir
base = Path.dirPath "/sys/bus/pci/devices/"

-- TODO: Check if reading then writing is not somehow out-of-ordered.
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
    path = base </> Path.relPath (Text.unpack bdfT) </> Path.filePath "config"
    bdfT = unBusDeviceFunction bdf
    cmdRegOffset = 4
    busMasterEnableIndex = 2
    setDMA b = (B.head b .|. shift 1 busMasterEnableIndex) `B.cons` B.tail b

mapResource :: (MonadIO m) => BusDeviceFunction -> Text -> m (Ptr a)
mapResource bdf resource =
    liftIO $ do
        unbind bdf
        enableDMA bdf
        PathIO.withBinaryFile
            path
            PathIO.ReadWriteMode
            (\h ->
                 liftIO $ do
                     size <- PathIO.hFileSize h
                     fd <- handleToFd h
                     memoryMap
                         Nothing
                         (fromIntegral size)
                         [MemoryProtectionRead, MemoryProtectionWrite]
                         MemoryMapShared
                         (Just fd)
                         0)
  where
    res = Path.relFile $ Text.unpack resource
    path = base </> Path.relPath (Text.unpack bdfT) </> res
    bdfT = unBusDeviceFunction bdf

unbind :: (MonadCatch m, MonadIO m) => BusDeviceFunction -> m ()
unbind bdf = handleIf isDoesNotExistError handler (liftIO $ PathIO.writeFile path $ Text.unpack bdfT)
  where
    path = base </> Path.relPath (Text.unpack bdfT) </> Path.filePath "driver/unbind"
    bdfT = unBusDeviceFunction bdf
    handler _ = return ()
