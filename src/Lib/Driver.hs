-- |
-- Module      :  Lib.Driver
-- Copyright   :  Alex Egger 2018
-- License     :  BSD3
--
-- Maintainer  :  alex.egger96@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Description
--
module Lib.Driver
  ( QueueId(QueueId)
  , Device(..)
  , Stats(..)
  )
where

import           Lib.Prelude

import qualified Data.Vector                   as V

-- | An integer-based ID of a rx or tx queue.
newtype QueueId = QueueId Int

-- | A collection of functions that can be executed on a device by a driver.
data Device = Device { send :: QueueId -> V.Vector ByteString -> IO (Either (V.Vector ByteString) ())
                     , receive :: QueueId -> Int -> IO (V.Vector ByteString)
                     , stats :: IO Stats
                     , setPromisc :: Bool -> IO ()
                     , dump :: IO Text }

-- | Holds the amount of packets that were send/received by a device.
-- 
-- Exact meaning of the fields is device-dependant.
data Stats = Stats {stRxPkts :: Int
                   , stTxPkts :: Int} deriving (Show)
