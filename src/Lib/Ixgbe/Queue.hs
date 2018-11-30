-- |
-- Module      :  Lib.Ixgbe.Queue
-- Copyright   :  Alex Egger 2018
-- License     :  BSD3
--
-- Maintainer  :  alex.egger96@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Description
--

module Lib.Ixgbe.Queue
  ( RxQueue(..)
  , TxQueue(..)
  , ReceiveDescriptor(..)
  , TransmitDescriptor(..)
  , mkRxQueue
  , mkTxQueue
  , numRxQueueEntries
  , numTxQueueEntries
  , nullReceiveDescriptor
  , isDone
  , isEndOfPacket
  , nullTransmitDescriptor
  , bufferSize
  , rxMap
  , rxGetMapping
  , txMap
  , txGetMapping
  )
where

import           Lib.Memory
import           Lib.Prelude

import           Control.Monad.Catch            ( MonadThrow )
import           Control.Monad.Logger
import           Data.IORef
import qualified Data.Vector.Unboxed           as Unbox
import           Foreign.Ptr                    ( castPtr
                                                , plusPtr
                                                )
import           Foreign.Storable               ( sizeOf
                                                , alignment
                                                , peek
                                                , poke
                                                , peekByteOff
                                                , pokeByteOff
                                                )
import           Foreign.Marshal.Utils          ( fillBytes )

numRxQueueEntries :: Int
numRxQueueEntries = 512

numTxQueueEntries :: Int
numTxQueueEntries = 512

bufferSize :: Int
bufferSize = 2048

-- $ Queues

data RxQueue = RxQueue { rxqDescriptor :: Int -> Ptr ReceiveDescriptor
                       , rxqMemPool :: !MemPool
                       , rxqMapRef :: !(IORef (Unbox.Vector Int))
                       , rxqIndexRef :: !(IORef Int)
                       }

mkRxQueue :: (MonadThrow m, MonadIO m, MonadLogger m) => m RxQueue
mkRxQueue = do
  -- Setup the descriptors and buffers.
  memPool <- mkMemPool $ numRxQueueEntries + numTxQueueEntries
  descPtr <- allocateDescriptors
    (numRxQueueEntries * sizeOf nullReceiveDescriptor)
  let descriptor i = descPtr `plusPtr` (i * sizeOf nullReceiveDescriptor)
  ids <- mapM (setupDescriptor memPool)
              [ descriptor i | i <- [0 .. numRxQueueEntries - 1] ]
  indexRef <- liftIO $ newIORef (0 :: Int)
  mapRef   <- liftIO $ newIORef $ Unbox.fromList ids
  return $! RxQueue
    { rxqDescriptor = descriptor
    , rxqMemPool    = memPool
    , rxqMapRef     = mapRef
    , rxqIndexRef   = indexRef
    }
 where
  setupDescriptor memPool ptr = liftIO $ do
    buf <- peek =<< allocateBuf memPool
    let PhysAddr physAddr = pbAddr buf
    poke ptr ReceiveRead {rdBufPhysAddr = physAddr, rdHeaderAddr = 0}
    return $ pbId buf

rxMap :: RxQueue -> Int -> Int -> IO ()
rxMap queue index id =
  modifyIORef' (rxqMapRef queue) (`Unbox.unsafeUpd` [(index, id)])

rxGetMapping :: RxQueue -> Int -> IO Int
rxGetMapping queue index = do
  rxqMap <- readIORef (rxqMapRef queue)
  return $ rxqMap Unbox.! (index `mod` numRxQueueEntries)


data TxQueue = TxQueue { txqDescriptor :: Int -> Ptr TransmitDescriptor
                       , txqMapRef :: !(IORef (Unbox.Vector Int))
                       , txqIndexRef :: !(IORef Int)
                       , txqCleanRef :: !(IORef Int)}

mkTxQueue :: (MonadThrow m, MonadIO m, MonadLogger m) => m TxQueue
mkTxQueue = do
  descPtr <- allocateDescriptors
    (numTxQueueEntries * sizeOf nullTransmitDescriptor)
  indexRef <- liftIO $ newIORef (0 :: Int)
  cleanRef <- liftIO $ newIORef (0 :: Int)
  mapRef   <- liftIO $ newIORef $ Unbox.generate numTxQueueEntries identity
  let descriptor i = descPtr `plusPtr` (i * sizeOf nullTransmitDescriptor)
  return $! TxQueue
    { txqDescriptor = descriptor
    , txqMapRef     = mapRef
    , txqIndexRef   = indexRef
    , txqCleanRef   = cleanRef
    }

txMap :: TxQueue -> Int -> Int -> IO ()
txMap queue index id =
  modifyIORef' (txqMapRef queue) (`Unbox.unsafeUpd` [(index, id)])

txGetMapping :: TxQueue -> Int -> IO Int
txGetMapping queue index = do
  txqMap <- readIORef (txqMapRef queue)
  return $ txqMap Unbox.! (index `mod` numTxQueueEntries)

-- $ Descriptors

data ReceiveDescriptor = ReceiveRead { rdBufPhysAddr :: {-# UNPACK #-} !Word64
                                     , rdHeaderAddr :: {-# UNPACK #-} !Word64 }
                         | ReceiveWriteback { rdStatus :: {-# UNPACK #-} !Word32
                                            , rdLength :: {-# UNPACK #-} !Word16}

instance Storable ReceiveDescriptor where
  sizeOf _ = 16
  alignment = sizeOf
  peek ptr = do
    status <- peekByteOff ptr 8
    len <- peekByteOff ptr 12
    return ReceiveWriteback {rdStatus=status, rdLength=len}
  poke ptr (ReceiveRead bufPhysAddr headerAddr) = do
    poke (castPtr ptr) bufPhysAddr
    pokeByteOff ptr 8 headerAddr
  poke _ (ReceiveWriteback _ _) = return $ panic "Cannot poke a writeback descriptor."

nullReceiveDescriptor :: ReceiveDescriptor
nullReceiveDescriptor = ReceiveRead {rdBufPhysAddr = 0, rdHeaderAddr = 0}

isDone :: ReceiveDescriptor -> Bool
isDone desc = testBit (rdStatus desc) 0

isEndOfPacket :: ReceiveDescriptor -> Bool
isEndOfPacket desc = testBit (rdStatus desc) 1

data TransmitDescriptor = TransmitRead { tdBufPhysAddr :: {-# UNPACK #-} !Word64
                                       , tdCmdTypeLen :: {-# UNPACK #-} !Word32
                                       , tdOlInfoStatus :: {-# UNPACK #-} !Word32 }
                          | TransmitWriteback { tdStatus :: {-# UNPACK #-} !Word32 }

instance Storable TransmitDescriptor where
  sizeOf _ = 16
  alignment = sizeOf
  peek ptr = do
    status <- peekByteOff ptr 12
    return TransmitWriteback {tdStatus = status}
  poke ptr (TransmitRead bufPhysAddr cmdTypeLen olInfoStatus) = do
    poke (castPtr ptr) bufPhysAddr
    pokeByteOff ptr 8 cmdTypeLen
    pokeByteOff ptr 12 olInfoStatus
  poke _ (TransmitWriteback _) = return $ panic "Cannot poke a writeback descriptor."

nullTransmitDescriptor :: TransmitDescriptor
nullTransmitDescriptor =
  TransmitRead {tdBufPhysAddr = 0, tdCmdTypeLen = 0, tdOlInfoStatus = 0}

-- $ Memory

allocateDescriptors
  :: (MonadThrow m, MonadIO m, MonadLogger m) => Int -> m (Ptr a)
allocateDescriptors size = do
  descPtr <- allocateMem size True
  liftIO $ fillBytes descPtr 0xFF size
  return descPtr
