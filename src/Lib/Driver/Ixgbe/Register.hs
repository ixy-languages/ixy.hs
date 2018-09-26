{-# LANGUAGE FlexibleContexts #-}

module Lib.Driver.Ixgbe.Register
    ( Register(..)
    , clearMask
    , set
    , setMask
    , get
    , waitClear
    ) where

import Lib.Driver.Ixgbe.Device
import Lib.Log (Logger, logLn)
import Lib.Prelude hiding (get, mask)

import qualified Control.Monad.State as S
import Data.Bits ((.&.), (.|.), complement)
import Foreign.Storable (Storable, peekByteOff, pokeByteOff)
import System.Posix.Unistd (usleep)

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
    deriving (Show)

instance Enum Register where
    fromEnum EIMC = 0x00888
    fromEnum CTRL = 0x00000
    fromEnum AUTOC = 0x042A0
    fromEnum RXCTRL = 0x02F00
    fromEnum (RXPBSIZE i) = 0x03C00 + (i * 4)
    fromEnum HLREG0 = 0x04240
    fromEnum RDRXCTL = 0x02F00
    fromEnum FCTRL = 0x05080
    fromEnum (SRRCTL i)
        | i <= 15 = 0x02100 + (i * 4)
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
    fromEnum CTRL_EXT = 0x00018
    fromEnum (DCA_RXCTRL i)
        | i <= 15 = 0x02200 + (i * 4)
    fromEnum (DCA_RXCTRL i)
        | i < 64 = 0x0100C + (i * 0x40)
    fromEnum (DCA_RXCTRL i) = 0x0D00C + ((i - 64) * 0x40)
    fromEnum (TXPBSIZE i) = 0x0CC00 + (i * 4)
    fromEnum DTXMXSZRQ = 0x08100
    fromEnum RTTDCS = 0x04900

set :: (MonadIO m, Device m) => Register -> Word -> m ()
set reg value = do
    dev <- S.get
    liftIO $ pokeByteOff (basePtr dev) (fromEnum reg) value

get :: (MonadIO m, Device m) => Register -> m Word
get reg = do
    dev <- S.get
    liftIO $ peekByteOff (basePtr dev) (fromEnum reg)

waitClear :: (MonadIO m, Logger m, Device m) => Register -> Word -> m ()
waitClear reg mask = do
    logLn $ "Waiting for flags " <> show mask <> " in register " <> show reg <> " to clear."
    inner 1
  where
    inner 0 = return ()
    inner _ = do
        liftIO $ usleep 10000
        current <- get reg
        inner (current .&. mask)

setMask :: (MonadIO m, Device m) => Register -> Word -> m ()
setMask reg mask = do
    current <- get reg
    set reg (current .|. mask)

clearMask :: (MonadIO m, Device m) => Register -> Word -> m ()
clearMask reg mask = do
    current <- get reg
    set reg (current .&. complement mask)
