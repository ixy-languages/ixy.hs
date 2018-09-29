module Lib.Core
    ( Env(..)
    , LogType
    ) where

import Lib.Prelude

import System.Log.FastLogger (TimedFastLogger)

type LogType = (TimedFastLogger, IO ())

data Env = Env
    { envLogger :: LogType
    }
