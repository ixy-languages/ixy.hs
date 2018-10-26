-- |
-- Module      :  Lib.Driver.Ixgbe.Descriptor
-- Copyright   :  Alex Egger 2018
-- License     :  BSD3
--
-- Maintainer  :  alex.egger96@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- IXGBE Data Descriptors
--

module Lib.Driver.Ixgbe.Descriptor
  ( ReceiveDescriptor(..)
  , nullReceiveDescriptor
  , TransmitDescriptor(..)
  , nullTransmitDescriptor
  )
where

import           Lib.Prelude

import           Foreign.Ptr                    ( castPtr )
import           Foreign.Storable               ( sizeOf
                                                , alignment
                                                , peek
                                                , peekByteOff
                                                , poke
                                                , pokeByteOff
                                                )

nullReceiveDescriptor :: ReceiveDescriptor
nullReceiveDescriptor = ReceiveRead {rdBufPhysAddr = 0, rdHeaderAddr = 0}

data ReceiveDescriptor = ReceiveRead { rdBufPhysAddr :: Word64
                                     , rdHeaderAddr :: Word64 }
                       | ReceiveWriteback { rdStatus :: Word32
                                          , rdLength :: Word16}

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

nullTransmitDescriptor :: TransmitDescriptor
nullTransmitDescriptor =
  TransmitRead {tdBufPhysAddr = 0, tdCmdTypeLen = 0, tdOlInfoStatus = 0}

data TransmitDescriptor = TransmitRead { tdBufPhysAddr :: Word64
                                       , tdCmdTypeLen  :: Word32
                                       , tdOlInfoStatus :: Word32
                                       }
                        | TransmitWriteback {tdStatus :: Word32}

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
