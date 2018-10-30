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
  , resetReceiveDescriptor
  , numRxQueueEntries
  , TxQueue(..)
  , mkTxQueueM
  , setTransmitDescriptor
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

data RxQueue = RxQueue { rxqDescriptor :: Int -> Ptr ReceiveDescriptor
                       , rxqBuffer :: Int -> (Ptr Word8, PhysAddr)
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
      descriptor i = descBase `plusPtr` (i * sizeOf nullReceiveDescriptor)
      buffer i =
        let bufPtr      = bufBase `plusPtr` (i * bufferSize)
            bufPhysAddr = bufPhysBase + fromIntegral (i * bufferSize)
        in  (bufPtr, bufPhysAddr)
  indexRef <- liftIO $ newIORef (0 :: Int)
  return RxQueue
    { rxqDescriptor = descriptor
    , rxqBuffer     = buffer
    , rxqIndexRef   = indexRef
    }
 where
  writeDescriptor ptr (offset, bufPhysAddr) = do
    let descPtr = ptr `plusPtr` offset
    liftIO $ poke descPtr
                  ReceiveRead {rdBufPhysAddr = bufPhysAddr, rdHeaderAddr = 0}

resetReceiveDescriptor :: RxQueue -> Int -> PhysAddr -> IO ()
resetReceiveDescriptor queue index bufPhysAddr = poke
  (rxqDescriptor queue index)
  ReceiveRead {rdBufPhysAddr = bufPhysAddr, rdHeaderAddr = 0}

-- $ Transmit Queues

data TxQueue = TxQueue { txqDescriptor :: Int -> Ptr TransmitDescriptor
                       , txqBuffer :: Int -> (Ptr Word8, PhysAddr)
                       , txqIndexRef :: IORef Int
                       , txqCleanRef :: IORef Int}

mkTxQueueM :: (MonadIO m) => Ptr TransmitDescriptor -> Ptr Word8 -> m TxQueue
mkTxQueueM descBase bufBase = do
  bufPhysBase <- translate bufBase
  indexRef    <- liftIO $ newIORef (0 :: Int)
  cleanRef    <- liftIO $ newIORef (0 :: Int)
  let descriptor i = descBase `plusPtr` (i * sizeOf nullTransmitDescriptor)
      buffer i =
        let bufPtr      = bufBase `plusPtr` (i * bufferSize)
            bufPhysAddr = bufPhysBase + fromIntegral (i * bufferSize)
        in  (bufPtr, bufPhysAddr)
  return TxQueue
    { txqDescriptor = descriptor
    , txqBuffer     = buffer
    , txqIndexRef   = indexRef
    , txqCleanRef   = cleanRef
    }

setTransmitDescriptor :: TxQueue -> Int -> PhysAddr -> Int -> IO ()
setTransmitDescriptor queue index bufPhysAddr size = poke
  (txqDescriptor queue index)
  TransmitRead
    { tdBufPhysAddr  = bufPhysAddr
    , tdCmdTypeLen   = fromIntegral $ cmdTypeLen size
    , tdOlInfoStatus = fromIntegral $ shift size 14
    }
 where
  cmdTypeLen = (.|.)
    (endOfPacket .|. reportStatus .|. frameCheckSequence .|. descExt .|. advDesc
    )
   where
    endOfPacket        = 0x1000000
    reportStatus       = 0x8000000
    frameCheckSequence = 0x2000000
    descExt            = 0x20000000
    advDesc            = 0x300000
