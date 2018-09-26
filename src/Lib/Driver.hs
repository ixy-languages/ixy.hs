module Lib.Driver
    ( Device
    , numRxQueue
    , numTxQueue
    , sendBatch
    , receiveBatch
    , setPromiscous
    , linkSpeed
    ) where

import Lib.Memory (PacketBuf)
import Lib.Prelude

import Foreign.Ptr (Ptr)

class (Monad m) =>
      Driver m
    where
    numRxQueue :: m Int
    numTxQueue :: m Int
    sendBatch :: Int -> [Ptr PacketBuf] -> m ()
    receiveBatch :: Int -> m [Ptr PacketBuf]
    setPromiscous :: Bool -> m ()
    linkSpeed :: m Int
