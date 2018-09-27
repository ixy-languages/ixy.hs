{-|
Module      : Lib
Description : Lib's main module

This is a haddock comment describing your library
For more information on how to write Haddock comments check the user guide:
<https://www.haskell.org/haddock/doc/html/index.html>
-}
module Lib
    ( init
    , Env(..)
    , Dev(..)
    , BusDeviceFunction(..)
    , busDeviceFunction
    ) where

import Lib.Core (Env(..))
import Lib.Ixgbe (init)
import Lib.Ixgbe.Types (Dev(..))
import Lib.Pci.Types (BusDeviceFunction(..), busDeviceFunction)
