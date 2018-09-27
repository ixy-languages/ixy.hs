module Lib.Ixgbe.Types.Extended
    ( Device(..)
    ) where

import Lib.Core (Env(..))
import Lib.Ixgbe.Types

class Device a where
    getDevice :: a -> Dev

instance Device Env where
    getDevice = envDevice
