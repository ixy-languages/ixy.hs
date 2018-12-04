{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import           Lib

import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Logger
import           Data.IORef
import           Data.Maybe
import           Foreign.Ptr                    ( castPtr )
import           Foreign.Storable               ( poke
                                                , peek
                                                )
import           Protolude
import           System.Clock

newtype App a = App { runApp :: LoggingT IO a } deriving (Functor, Applicative, Monad, MonadIO, MonadCatch, MonadThrow, MonadLogger)

main :: IO ()
main = runStdoutLoggingT (runApp run)

run :: App ()
run = do
  dev1    <- fromJust <$> newDriver "0000:02:00.0" 1 1
  dev2    <- fromJust <$> newDriver "0000:02:00.1" 1 1
  counter <- liftIO $ newIORef (0 :: Int)
  liftIO $ loop counter dev1 dev2

loop :: IORef Int -> Device -> Device -> IO ()
loop counter dev1 dev2 = do
  let clock = Monotonic
  timeRef <- newIORef (TimeSpec {sec = 0, nsec = 0})
  forever $ do
    forward dev1 dev2
    forward dev2 dev1
    !c <- readIORef counter
    when
      (c .&. 0xF == 0)
      (do
        !t          <- getTime clock
        !beforeTime <- readIORef timeRef
        let diffTime = diffTimeSpec t beforeTime
        when
          (sec diffTime >= 1)
          (do
            putStrLn ("Diff was: " <> show diffTime :: Text)
            !st1 <- stats dev1
            !st2 <- stats dev2
            let mult =
                  fromIntegral (sec diffTime)
                    + (fromIntegral (nsec diffTime) / 1.0e9) :: Float
                divisor = 1000000 * mult
            putStrLn
              ("Driver 1 -> RX: "
              <> show (fromIntegral (stRxPkts st1) / divisor)
              <> "Mpps | TX: "
              <> show (fromIntegral (stTxPkts st1) / divisor)
              <> "Mpps" :: Text
              )
            putStrLn
              ("Driver 2 -> RX: "
              <> show (fromIntegral (stRxPkts st2) / divisor)
              <> " Mpps | TX: "
              <> show (fromIntegral (stTxPkts st2) / divisor)
              <> "Mpps" :: Text
              )
            writeIORef timeRef t
          )
      )
    modifyIORef' counter (+ 1)

forward :: Device -> Device -> IO ()
forward rxDev txDev = do
  !pkts <- receive rxDev 0 128
  mapM_ touchPacket pkts
  send txDev 0 (memPoolOf rxDev 0) (reverse pkts)
 where
  touchPacket ptr =
    poke (castPtr ptr) =<< (+ 1) <$> (peek (castPtr ptr) :: IO Int)
