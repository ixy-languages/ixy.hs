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

newtype QueueId = QueueId Int

data Device = Device { send :: QueueId -> V.Vector ByteString -> IO (Either (V.Vector ByteString) ())
                     , receive :: QueueId -> Int -> IO (V.Vector ByteString)
                     , stats :: IO Stats
                     , setPromisc :: Bool -> IO ()
                     , dump :: IO Text }

data Stats = Stats {stRxPkts :: Int
                   , stTxPkts :: Int} deriving (Show)
