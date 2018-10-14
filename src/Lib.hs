-- |
-- Module      :  Lib
-- Copyright   :  Alex Egger 2018
-- License     :  BSD3
--
-- Maintainer  :  alex.egger96@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Description
--
module Lib
  ( newDriver
  , receive
  , send
  , stats
  )
where

import           Lib.Driver
import           Lib.Driver.Ixgbe
import           Lib.Driver.Ixgbe.Types
import           Lib.Pci                        ( BusDeviceFunction
                                                , busDeviceFunction
                                                )
import           Lib.Prelude

import           Control.Monad.Catch
import           Control.Monad.Logger           ( MonadLogger )
import           Data.Maybe                     ( fromMaybe )
import           System.IO.Error                ( userError )

newDriver
  :: (MonadCatch m, MonadThrow m, MonadIO m, MonadLogger m)
  => Text
  -> Int
  -> Int
  -> m (Maybe Device)
newDriver bdfT numRx numTx = case busDeviceFunction bdfT of
  Just bdf -> do
    dev <- init bdf numRx numTx
    return $ Just dev

  Nothing -> return Nothing
