{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import           Lib

import           Control.Monad.Catch
import           Control.Monad.Logger
import           Data.ByteString.Builder
import qualified Data.ByteString.Char8         as BC
import           Data.IORef
import           Data.Maybe
import qualified Data.Text                     as T
import           Protolude
import           System.Posix.Unistd            ( usleep )

newtype App a = App { runApp :: LoggingT IO a } deriving (Functor, Applicative, Monad, MonadIO, MonadCatch, MonadThrow, MonadLogger)

main :: IO ()
main = runStdoutLoggingT (runApp run)

run :: App ()
run = do
  dev1 <- fromJust <$> newDriver "0000:02:00.0" 1 1
  dev2 <- fromJust <$> newDriver "0000:02:00.1" 1 1
  loop dev1 dev2

  -- readPackets dev
 -- where
  -- readPackets = evalStateT
  --   (forever $ do
  --     packets <- receive 0 32
  --     let b = map (toLazyByteString . byteStringHex) packets
  --     if length b > 0
  --       then do
  --         liftIO $ putStrLn (show b :: Text)
  --         _ <- send 0 packets
  --         return ()
  --       else return ()
  --   )

loop :: Device -> Device -> App ()
loop dev1 dev2 = do
  devRefs <- liftIO $ newIORef ((dev1, dev2) :: (Device, Device))
  forever $ do
    devices  <- liftIO $ readIORef devRefs
    devices' <- execStateT forward devices
    liftIO $ writeIORef devRefs devices'


forward :: StateT (Device, Device) App ()
forward = do
  (rxDev, txDev ) <- get
  (pkts , rxDev') <- runStateT (receive 0 32) rxDev
  if not (null pkts)
    then do
      -- $(logDebug)
      --   $  "["
      --   <> unBusDeviceFunction (_devBdf rxDev)
      --   <> " -> "
      --   <> unBusDeviceFunction (_devBdf txDev)
      --   <> "] Forwarding packets..."
      txDev' <- execStateT (send 0 pkts) txDev
      txStat <- runReaderT stats txDev
      $(logDebug) $ show txStat
      put (rxDev', txDev')
    else put (rxDev', txDev)
