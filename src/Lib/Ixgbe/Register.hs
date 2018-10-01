{-# LANGUAGE FlexibleContexts #-}

module Lib.Ixgbe.Register
    ( Register(..)
    , clearMask
    , set
    , waitSet
    , setMask
    , get
    , waitClear
    ) where

import Lib.Ixgbe.Types (Device(..), DeviceState)
import Lib.Log (Logger, halt, logLn)
import Lib.Prelude hiding (get, mask)

import qualified Control.Monad.State as State
import Data.Bits ((.&.), (.|.), complement)
import Foreign.Storable (peekByteOff, pokeByteOff)
import System.IO.Error (userError)
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
    fromEnum GPRC = 0x04074
    fromEnum GPTC = 0x04080
    fromEnum GORCL = 0x04088
    fromEnum GORCH = 0x0408C
    fromEnum GOTCL = 0x04090
    fromEnum GOTCH = 0x04094
    fromEnum (TDBAL i) = 0x06000 + (i * 0x40)
    fromEnum (TDBAH i) = 0x06004 + (i * 0x40)
    fromEnum (TDLEN i) = 0x06008 + (i * 0x40)
    fromEnum (TXDCTL i) = 0x06028 + (i * 0x40)
    fromEnum DMATXCTL = 0x04A80
    fromEnum EEC = 0x10010
    fromEnum (TDH i) = 0x06010 + (i * 0x40)
    fromEnum (TDT i) = 0x06018 + (i * 0x40)
    fromEnum LINKS = 0x042A4
    fromEnum (RXDCTL i)
        | i < 64 = 0x01028 + (i * 0x40)
    fromEnum (RXDCTL i) = 0x0D028 + ((i - 64) * 0x40)
    fromEnum UNDEFINED = -1
    toEnum _ = UNDEFINED

set :: (MonadIO m, MonadReader env m, Logger env, DeviceState m) => Register -> Word32 -> m ()
set UNDEFINED _ = halt "Logical Error" $ userError "Not supposed to convert from Int to Register, or use UNDEFINED directly."
set reg value = do
    dev <- State.get
    let ptr = devBase dev
     in liftIO $ pokeByteOff ptr (fromEnum reg) value

get :: (MonadIO m, MonadReader env m, Logger env, DeviceState m) => Register -> m Word32
get UNDEFINED = halt "Logical Error" $ userError "Not supposed to convert from Int to Register, or use UNDEFINED directly."
get reg = do
    dev <- State.get
    let ptr = devBase dev
     in liftIO $ peekByteOff ptr (fromEnum reg)

waitClear :: (MonadIO m, MonadReader env m, Logger env, DeviceState m) => Register -> Word32 -> m ()
waitClear reg mask = do
    logLn $ "Waiting for flags " <> show mask <> " in register " <> show reg <> " to clear."
    waitUntil reg mask (== 0)

waitSet :: (MonadIO m, MonadReader env m, Logger env, DeviceState m) => Register -> Word32 -> m ()
waitSet reg mask = do
    logLn $ "Waiting for flags " <> show mask <> " in register " <> show reg <> " to be set."
    waitUntil reg mask (== mask)

waitUntil :: (MonadIO m, MonadReader env m, Logger env, DeviceState m) => Register -> Word32 -> (Word32 -> Bool) -> m ()
waitUntil reg mask f = do
    current <- get reg
    if f $ current .&. mask
        then return ()
        else do
            liftIO $ usleep 10000
            waitUntil reg mask f

setMask :: (MonadIO m, MonadReader env m, Logger env, DeviceState m) => Register -> Word32 -> m ()
setMask reg mask = do
    current <- get reg
    set reg (current .|. mask)

clearMask :: (MonadIO m, MonadReader env m, Logger env, DeviceState m) => Register -> Word32 -> m ()
clearMask reg mask = do
    current <- get reg
    set reg (current .&. complement mask)
