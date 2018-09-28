module Lib.Pci.Types
    ( BusDeviceFunction(unBusDeviceFunction)
    , busDeviceFunction
    ) where

import Lib.Prelude

import qualified Data.Text as T
import Text.Regex.PCRE ((=~))

newtype BusDeviceFunction = BDF
    { unBusDeviceFunction :: Text
    } deriving (Show)

busDeviceFunction :: Text -> Maybe BusDeviceFunction
busDeviceFunction bdfText
    | T.unpack bdfText =~ T.unpack "[0-9A-F]{4}:[0-9A-F]{2}:[0-9A-F]{2}.[0-9]" = Just (BDF bdfText)
busDeviceFunction _ = Nothing
