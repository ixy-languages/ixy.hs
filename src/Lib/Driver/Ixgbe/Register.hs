-- |
-- Module      :  Lib.Driver.Ixgbe.Register
-- Copyright   :  Alex Egger 2018
-- License     :  BSD3
--
-- Maintainer  :  alex.egger96@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Description
--

module Lib.Driver.Ixgbe.Register
  ( Register(..)
  , set
  , get
  , setMask
  , clearMask
  , waitSet
  , waitClear
  )
where

import           Lib.Prelude             hiding ( get
                                                , mask
                                                )
import           Lib.Driver.Ixgbe.Types

import           Control.Lens            hiding ( set )
import           Data.Bits                      ( (.&.)
                                                , (.|.)
                                                , complement
                                                )
import           Foreign.Storable               ( peekByteOff
                                                , pokeByteOff
                                                )
import           System.Posix.Unistd            ( usleep )

data Register
    = EIMC
    | CTRL
    | AUTOC
    | RXCTRL
    | RXPBSIZE Int
    | HLREG0
    | RDRXCTL
    | FCTRL
    | SRRCTL Int
    | RDBAL Int
    | RDBAH Int
    | RDLEN Int
    | RDH Int
    | RDT Int
    | CTRL_EXT
    | DCA_RXCTRL Int
    | TXPBSIZE Int
    | DTXMXSZRQ
    | RTTDCS
    | GPRC
    | GPTC
    | GORCL
    | GORCH
    | GOTCL
    | GOTCH
    | TDBAL Int
    | TDBAH Int
    | TDLEN Int
    | TXDCTL Int
    | DMATXCTL
    | EEC
    | TDH Int
    | TDT Int
    | LINKS
    | RXDCTL Int
    | UNDEFINED
    deriving (Eq, Show)

instance Enum Register where
    fromEnum EIMC = 0x00888
    fromEnum CTRL = 0x00000
    fromEnum CTRL_EXT = 0x00018
    fromEnum (SRRCTL i)
        | i < 64 = 0x01014 + (i * 0x40)
    fromEnum (SRRCTL i) = 0x0D014 + ((i - 64) * 0x40)
    fromEnum (RDBAL i)
        | i < 64 = 0x01000 + (i * 0x40)
    fromEnum (RDBAL i) = 0x0D000 + ((i - 64) * 0x40)
    fromEnum (RDBAH i)
        | i < 64 = 0x01004 + (i * 0x40)
    fromEnum (RDBAH i) = 0x0D004 + ((i - 64) * 0x40)
    fromEnum (RDLEN i)
        | i < 64 = 0x1008 + (i * 0x40)
    fromEnum (RDLEN i) = 0x0D008 + ((i - 64) * 0x40)
    fromEnum (RDH i)
        | i < 64 = 0x1010 + (i * 0x40)
    fromEnum (RDH i) = 0x0D010 + ((i - 64) * 0x40)
    fromEnum (RDT i)
        | i < 64 = 0x1018 + (i * 0x40)
    fromEnum (RDT i) = 0x0D018 + ((i - 64) * 0x40)
    fromEnum (DCA_RXCTRL i)
        | i < 64 = 0x0100C + (i * 0x40)
    fromEnum (DCA_RXCTRL i) = 0x0D00C + ((i - 64) * 0x40)
    fromEnum (RXDCTL i)
        | i < 64 = 0x01028 + (i * 0x40)
    fromEnum (RXDCTL i) = 0x0D028 + ((i - 64) * 0x40)
    fromEnum RDRXCTL = 0x02F00
    fromEnum RXCTRL = 0x03000
    fromEnum (RXPBSIZE i) = 0x03C00 + (i * 4)
    fromEnum GPRC = 0x04074
    fromEnum GPTC = 0x04080
    fromEnum GORCL = 0x04088
    fromEnum GORCH = 0x0408C
    fromEnum GOTCL = 0x04090
    fromEnum GOTCH = 0x04094
    fromEnum HLREG0 = 0x04240
    fromEnum AUTOC = 0x042A0
    fromEnum LINKS = 0x042A4
    fromEnum RTTDCS = 0x04900
    fromEnum DMATXCTL = 0x04A80
    fromEnum FCTRL = 0x05080
    fromEnum (TDBAL i) = 0x06000 + (i * 0x40)
    fromEnum (TDBAH i) = 0x06004 + (i * 0x40)
    fromEnum (TDLEN i) = 0x06008 + (i * 0x40)
    fromEnum (TDH i) = 0x06010 + (i * 0x40)
    fromEnum (TDT i) = 0x06018 + (i * 0x40)
    fromEnum (TXDCTL i) = 0x06028 + (i * 0x40)
    fromEnum DTXMXSZRQ = 0x08100
    fromEnum (TXPBSIZE i) = 0x0CC00 + (i * 4)
    fromEnum EEC = 0x10010

set :: (MonadIO m, MonadReader Device m) => Register -> Word32 -> m ()
set register value = do
  dev <- ask
  liftIO $ pokeByteOff (dev ^. devBase) (fromEnum register) value

get :: (MonadIO m, MonadReader Device m) => Register -> m Word32
get register = do
  dev <- ask
  liftIO $ peekByteOff (dev ^. devBase) $ fromEnum register

setMask :: (MonadIO m, MonadReader Device m) => Register -> Word32 -> m ()
setMask register mask = do
  current <- get register
  set register (current .|. mask)

clearMask :: (MonadIO m, MonadReader Device m) => Register -> Word32 -> m ()
clearMask register mask = do
  current <- get register
  set register (current .|. complement mask)

waitUntil
  :: (MonadIO m, MonadReader Device m)
  => Register
  -> Word32
  -> (Word32 -> Bool)
  -> m ()
waitUntil register mask f = do
  current <- get register
  if f $ current .&. mask
    then return ()
    else do
      liftIO $ usleep 10000
      waitUntil register mask f

waitSet :: (MonadIO m, MonadReader Device m) => Register -> Word32 -> m ()
waitSet register mask = waitUntil register mask (== mask)

waitClear :: (MonadIO m, MonadReader Device m) => Register -> Word32 -> m ()
waitClear register mask = waitUntil register mask (== 0)
--     toEnum 0x00888 = EIMC
--     toEnum 0x00000 = CTRL
--     toEnum 0x042A0 = AUTOC
--     toEnum 0x02F00 = RDRXCTL
--     toEnum v
--         | v >= 0x03C00 && v <= 0x03C1C = RXPBSIZE ((v - 0x03C00) `quot` 4)
--     toEnum v
--         | v >= 0x0CC00 && v <= 0xCC1C = TXPBSIZE ((v - 0x0CC00) `quot` 4)
--     toEnum 0x04240 = HLREG0
--     toEnum 0x03000 = RXCTRL
--     toEnum 0x05080 = FCTRL
--     toEnum v
--         | v >= 0x01000 && v <= 0x01FC0 && v `mod` 0x40 == 0 = RDBAL ((v - 0x01000) `quot` 0x40)
--     toEnum v
--         | v >= 0x0D000 && v <= 0x0DFC0 && v `mod` 0x40 == 0 = RDBAL (((v - 0x0D000) `quot` 0x40) + 64)
--     toEnum v
--         | v >= 0x01004 && v <= 0x01FC4 && v `mod` 0x40 == 4 = RDBAH ((v - 0x01004) `quot` 0x40)
--     toEnum v
--         | v >= 0x0D004 && v <= 0x0DFC4 && v `mod` 0x40 == 4 = RDBAH (((v - 0x0D004) `quot` 0x40) + 64)
--     toEnum v
--         | v >= 0x01008 && v <= 0x01FC8 && v `mod` 0x40 == 8 = RDLEN ((v - 0x01008) `quot` 0x40)
--     toEnum v
--         | v >= 0x0D008 && v <= 0x0DFC8 && v `mod` 0x40 == 8 = RDLEN (((v - 0x0D008) `quot` 0x40) + 64)
--     toEnum v
--         | v >= 0x01010 && v <= 0x01FD0 && v `mod` 0x40 == 0x10 = RDH ((v - 0x01014) `quot` 0x40)
--     toEnum v
--         | v >= 0x0D010 && v <= 0x0DFD0 && v `mod` 0x40 == 0x10 = RDH (((v - 0x0D014) `quot` 0x40) + 64)
--     toEnum v
--         | v >= 0x01014 && v <= 0x01FD4 && v `mod` 0x40 == 0x14 = SRRCTL ((v - 0x01014) `quot` 0x40)
--     toEnum v
--         | v >= 0x0D014 && v <= 0x0D0F4 && v `mod` 0x40 == 0x14 = SRRCTL (((v - 0x0D014) `quot` 0x40) + 64)
--     toEnum v
--         | v >= 0x01018 && v <= 0x01FD8 && v `mod` 0x40 == 0x18 = RDT ((v - 0x01018) `quot` 0x40)
--     toEnum v
--         | v >= 0x0D018 && v <= 0x0DFD8 && v `mod` 0x40 == 0x18 = RDT (((v - 0x0D018) `quot` 0x40) + 64)
--     toEnum 0x00018 = CTRL_EXT
--     toEnum v
--         | v >= 0x0100C && v <= 0x01FCC && v `mod` 0x40 == 0xC = DCA_RXCTRL ((v - 0x0100C) `quot` 0x40)
--     toEnum v
--         | v >= 0x0D00C && v <= 0x0DFCC && v `mod` 0x40 == 0xC = DCA_RXCTRL (((v - 0x0D00C) `quot` 0x40) + 64)
--     toEnum v
--         | v >= 0x01028 && v <= 0x01FE8 && v `mod` 0x40 == 0x28 = RXDCTL ((v - 0x01028) `quot` 0x40)
--     toEnum v
--         | v >= 0x0D028 && v <= 0x0DFE8 && v `mod` 0x40 == 0x28 = RXDCTL (((v - 0x0D028) `quot` 0x40) + 64)
--     toEnum 0x08100 = DTXMXSZRQ
--     toEnum 0x04900 = RTTDCS
--     toEnum 0x04074 = GPRC
--     toEnum 0x04080 = GPTC
--     toEnum 0x04088 = GORCL
--     toEnum 0x0408C = GORCH
--     toEnum 0x04A80 = DMATXCTL
--     toEnum 0x10010 = EEC
--     toEnum 0x042A4 = LINKS
--     toEnum v
--         | v >= 0x06000 && v <= 0x07FC0 && v `mod` 0x40 == 0 = TDBAL ((v - 0x06000) `quot` 0x40)
--     toEnum v
--         | v >= 0x06004 && v <= 0x07FC4 && v `mod` 0x40 == 4 = TDBAH ((v - 0x06004) `quot` 0x40)
--     toEnum v
--         | v >= 0x06008 && v <= 0x07FC8 && v `mod` 0x40 == 8 = TDLEN ((v - 0x06008) `quot` 0x40)
--     toEnum v
--         | v >= 0x06010 && v <= 0x07FD0 && v `mod` 0x40 == 0x10 = TDH ((v - 0x06010) `quot` 0x40)
--     toEnum v
--         | v >= 0x06018 && v <= 0x07FD8 && v `mod` 0x40 == 0x18 = TDT ((v - 0x06018) `quot` 0x40)
--     toEnum v
--         | v >= 0x06028 && v <= 0x07FE8 && v `mod` 0x40 == 0x28 = TXDCTL ((v - 0x06028) `quot` 0x40)
--     toEnum _ = UNDEFINED
