{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Lib.Driver.Ixgbe.Device
    ( IxgbeDevice(..)
    , Device
    , basePtr
    , numRx
    , numTx
    , pciBDF
    ) where

import Lib.Log (Logger)
import Lib.Pci (BusDeviceFunction)
import Lib.Prelude

import Foreign.Ptr (Ptr)

-- This should be a MonadReader, but I can't find out how to use two at the same time :(
type Device = MonadState IxgbeDevice

data IxgbeDevice = IxgbeDevice
    { basePtr :: Ptr Word
    , pciBDF :: BusDeviceFunction
    , numRx :: Int
    , numTx :: Int
    }
