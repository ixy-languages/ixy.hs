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
    ) where

import Lib.Core (Env(..))
import Lib.Ixgbe (init)
