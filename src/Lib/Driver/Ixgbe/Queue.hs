-- |
-- Module      :  Lib.Driver.Ixgbe.Queue
-- Copyright   :  Alex Egger 2018
-- License     :  BSD3
--
-- Maintainer  :  alex.egger96@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Description  
--

module Lib.Driver.Ixgbe.Queue
  ( RxQueue(..)
  , mkRxQueueM
  , numRxQueueEntries
  , TxQueue(..)
  , mkTxQueueM
  , numTxQueueEntries
  , bufferSize
  )
where

import           Lib.Driver.Ixgbe.Descriptor
import           Lib.Memory                     ( translate
                                                , PhysAddr
                                                )
import           Lib.Prelude

import           Data.IORef
import           Foreign.Ptr                    ( plusPtr )
import           Foreign.Storable               ( sizeOf
                                                , poke
                                                )

bufferSize :: Int
bufferSize = 2048

numRxQueueEntries :: Int
numRxQueueEntries = 512

numTxQueueEntries :: Int
numTxQueueEntries = 512

data RxQueue = RxQueue { rxqDescBase :: Ptr ReceiveDescriptor
                       , rxqBufBase :: Ptr Word8
                       , rxqBufPhysBase :: PhysAddr
                       , rxqIndexRef :: IORef Int}

mkRxQueueM :: (MonadIO m) => Ptr ReceiveDescriptor -> Ptr Word8 -> m RxQueue
mkRxQueueM descBase bufBase = do
  -- Setup receive descriptors.
  let indices = [0 .. (numRxQueueEntries - 1)]
  bufPhysAddrs <- mapM translate
                       [ bufBase `plusPtr` (i * bufferSize) | i <- indices ]
  mapM_ (writeDescriptor descBase)
    $ zip [ i * sizeOf nullReceiveDescriptor | i <- indices ] bufPhysAddrs
  -- Setup queue.
  let bufPhysBase = fromMaybe (panic "RxQueue was handed invalid buffer base.")
        $ head bufPhysAddrs
  indexRef <- liftIO $ newIORef (0 :: Int)
  return RxQueue
    { rxqDescBase    = descBase
    , rxqBufBase     = bufBase
    , rxqBufPhysBase = bufPhysBase
    , rxqIndexRef    = indexRef
    }
 where
  writeDescriptor ptr (offset, bufPhysAddr) = do
    let descPtr = ptr `plusPtr` offset
    liftIO $ poke descPtr
                  ReceiveRead {rdBufPhysAddr = bufPhysAddr, rdHeaderAddr = 0}

data TxQueue = TxQueue { txqDescBase :: Ptr TransmitDescriptor
                       , txqBufBase :: Ptr Word8
                       , txqBufPhysBase :: PhysAddr
                       , txqIndexRef :: IORef Int
                       , txqCleanRef :: IORef Int}

mkTxQueueM :: (MonadIO m) => Ptr TransmitDescriptor -> Ptr Word8 -> m TxQueue
mkTxQueueM descBase bufBase = do
  bufPhysBase <- translate bufBase
  indexRef    <- liftIO $ newIORef (0 :: Int)
  cleanRef    <- liftIO $ newIORef (0 :: Int)
  return TxQueue
    { txqDescBase    = descBase
    , txqBufBase     = bufBase
    , txqBufPhysBase = bufPhysBase
    , txqIndexRef    = indexRef
    , txqCleanRef    = cleanRef
    }
