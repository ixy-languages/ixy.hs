{-# LANGUAGE TemplateHaskell #-}
-- |
-- Module      :  Lib.Driver.Ixgbe
-- Copyright   :  Alex Egger 2018
-- License     :  BSD3
--
-- Maintainer  :  alex.egger96@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Description
--

module Lib.Driver.Ixgbe
  ()
where

import           Lib.Prelude             hiding ( to
                                                , rotateL
                                                )
import           Lib.Driver
import           Lib.Pci
import           Lib.Memory
import           Lib.Driver.Ixgbe.Types
import qualified Lib.Driver.Ixgbe.Register     as R

import           Control.Monad.Catch
import           Control.Monad.Logger           ( MonadLogger
                                                , logDebug
                                                )
import           Control.Lens            hiding ( index
                                                , indices
                                                )
import qualified Data.ByteString               as B
import qualified Data.Text                     as T
import           Foreign.Marshal.Array          ( peekArray )
import           Foreign.Storable               ( sizeOf
                                                , poke
                                                , peek
                                                )
import           Foreign.Ptr                    ( plusPtr
                                                , castPtr
                                                )
import           Numeric                        ( showHex )
import qualified Data.Vector.Storable          as Storable
import qualified Data.Vector                   as V
import           System.IO.Error                ( userError )
import           System.Posix.Unistd            ( usleep )

numRxQueueEntries :: Int
numRxQueueEntries = 512

numTxQueueEntries :: Int
numTxQueueEntries = 512

instance Driver Device where
  init bdf numRx numTx = do
    $(logDebug) $ "Initializing device " <> unBusDeviceFunction bdf <> "."

    ptr <- mapResource bdf "resource0"
    execStateT inner $ Device {_devBase = ptr, _devBdf = bdf, _devRxQueues = V.empty, _devTxQueues = V.empty}
    where inner = do dev <- get
                     _ <- runReaderT (do reset
                                         R.waitSet R.EEC autoReadDone
                                         R.waitSet R.RDRXCTL dmaInitCycleDone
                                         initLink
                                         stats) dev
                     rxQueues <- runReaderT (initRx numRx) dev
                     put $ dev & devRxQueues .~ V.fromList rxQueues
                     -- txQueues <- runReaderT (initTx numTx) dev
                     -- put $ dev & devTxQueues .~ V.fromList txQueues
                     runReaderT (do setPromiscuous True
                                    waitForLink 1000) dev
                     where autoReadDone = 0x200
                           dmaInitCycleDone = 0x8
          reset = do R.set R.EIMC disableInterrupt
                     R.set R.CTRL resetMask
                     R.waitClear R.CTRL resetMask
                     R.set R.EIMC disableInterrupt
                       where disableInterrupt = 0x7FFFFFFF
                             resetMask = 0x4000008
          initLink = R.setMask R.AUTOC anRestart where anRestart = 0x1000

  stats = do
    rxPackets <- R.get R.GPRC
    txPackets <- R.get R.GPTC
    rxBytesL <- R.get R.GORCL
    rxBytesH <- (`shift` 32) <$> R.get R.GORCH
    txBytesL <- R.get R.GOTCL
    txBytesH <- (`shift` 32) <$> R.get R.GOTCH
    return Statistics
             { stRxPackets = fromIntegral rxPackets
             , stTxPackets = fromIntegral txPackets
             , stRxBytes = fromIntegral rxBytesL + fromIntegral rxBytesH
             , stTxBytes = fromIntegral txBytesL + fromIntegral txBytesH
             }

  setPromiscuous flag = do
    dev <- ask
    if flag then do $(logDebug) $ "Enabling promiscuous mode for device " <> unBusDeviceFunction (dev ^. devBdf) <> "."
                    R.setMask R.FCTRL promiscEnable
            else do $(logDebug) $ "Disabling promiscuous mode for device " <> unBusDeviceFunction (dev ^. devBdf) <> "."
                    R.clearMask R.FCTRL promiscEnable
    where promiscEnable = 0x300

  receive queueId numBufs = do
    dev <- get
    let queue = (dev ^. devRxQueues) V.! queueId
        ptrs = V.take numBufs $ V.zip (V.convert $ queue ^. rxqDescriptors) (V.convert $ queue ^. rxqBuffers)
         in do packets <- V.mapMaybe identity <$> mapM inner ptrs
               let queue' = queue & rxqDescriptors .~ rotateL (V.length packets) (queue ^. rxqDescriptors) & rxqBuffers .~ rotateL (V.length packets) (queue ^. rxqBuffers)
                   queues = (dev ^. devRxQueues) V.// [(queueId, queue')]
                    in do put $ dev & devRxQueues .~ queues
                          return $ V.toList packets
    where inner (descPtr, bufPtr) = do descriptor <- liftIO $ peek descPtr
                                       if isDone $ rdStatusError descriptor then
                                                                            if not $ isEndOfPacket $ rdStatusError descriptor then throwM $ userError "Multi-segment packets are not supported."
                                                                                                                              else let len = rdLength descriptor
                                                                                                                                        in do phys <- translate bufPtr
                                                                                                                                              liftIO $ poke descPtr ReadRx {rdPacketAddr=phys, rdHeaderAddr=0}
                                                                                                                                              buffer <- liftIO $ peekArray (fromIntegral len) bufPtr
                                                                                                                                              return $ Just (B.pack buffer)
                                                                            else return Nothing
           where isDone = flip testBit 1
                 isEndOfPacket = flip testBit 2

  send queueId buffers = undefined

rotateL :: (Storable a) => Int -> Storable.Vector a -> Storable.Vector a
rotateL n v = let (x, xs) = Storable.splitAt n v in xs Storable.++ x

initRx
  :: (MonadThrow m, MonadIO m, MonadReader Device m, MonadLogger m)
  => Int
  -> m [RxQueue]
initRx numRx = do
  $(logDebug) $ "Initializing " <> show numRx <> " rx queues."

  -- Disable RX for now.
  R.clearMask R.RXCTRL rxEnable

  -- Set buffer sizes.
  R.set (R.RXPBSIZE 0) bufferSize
  mapM_ (\i -> R.set (R.RXPBSIZE i) 0) [1 .. 7]

  R.setMask R.HLREG0 crcStrip
  R.setMask R.RDRXCTL crcStrip
  R.setMask R.FCTRL broadcastAcceptMode

  rxQueues <- mapM setupQueue [0 .. (numRx - 1)]

  R.setMask R.CTRL_EXT noSnoopDisable
  mapM_ (\i -> R.clearMask (R.DCA_RXCTRL $ fromIntegral i) $ shift 1 12)
        [0 .. (numRx - 1)]

  R.setMask R.RXCTRL rxEnable
  return rxQueues
 where
  rxEnable            = 0x1
  crcStrip            = 0x2
  broadcastAcceptMode = 0x400
  bufferSize          = 0x20000
  noSnoopDisable      = 0x10000
  setupQueue
    :: (MonadThrow m, MonadIO m, MonadReader Device m, MonadLogger m)
    => Int
    -> m RxQueue
  setupQueue index = do
    -- Enable advanced receive descriptors.
    advRxDescEnable <- fmap
      (.|. (0x02000000 :: Word32))
      ((.&. (0xF1FFFFFF :: Word32)) <$> R.get (R.SRRCTL index))
    R.set (R.SRRCTL index) advRxDescEnable
    -- Enable dropping of packets, if no descriptors are available.
    R.setMask (R.SRRCTL index) dropEnable

    let size = numRxQueueEntries * sizeOf (undefined :: ReceiveDescriptor)
    descPtr  <- allocateRaw size True
    physAddr <- translate descPtr
    R.set (R.RDBAL index) $ fromIntegral (physAddr .&. 0xFFFFFFFF)
    R.set (R.RDBAH index) $ fromIntegral (shift physAddr (-32))
    R.set (R.RDLEN index) $ fromIntegral size

    -- Enable the queue.
    R.setMask (R.RXDCTL index) rxdCtlEnable
    R.waitSet (R.RXDCTL index) rxdCtlEnable

    -- Set the queue to full.
    R.set (R.RDH index) 0
    R.set (R.RDT index) $ fromIntegral (numRxQueueEntries - 1)

    $(logDebug)
      $  "Rx Region "
      <> show index
      <> "at "
      <> show descPtr
      <> " (physical="
      <> T.pack (showHex physAddr "")
      <> ")."

    bufPtr <- allocateRaw (numRxQueueEntries * 2048) False
    let descPtrs =
          [ castPtr
              $         descPtr
              `plusPtr` (i * sizeOf (undefined :: ReceiveDescriptor))
          | i <- [0 .. (numRxQueueEntries - 1)]
          ]
        bufPtrs =
          [ castPtr $ bufPtr `plusPtr` (i * 2048)
          | i <- [0 .. (numRxQueueEntries - 1)]
          ]
    physAddrs <- mapM translate bufPtrs
    mapM_
        (\(ptr, bufPhysAddr) -> liftIO
          $ poke ptr ReadRx {rdPacketAddr = bufPhysAddr, rdHeaderAddr = 0}
        )
      $ zip descPtrs physAddrs
    return RxQueue
      { _rxqDescriptors = Storable.fromList descPtrs
      , _rxqBuffers     = Storable.fromList bufPtrs
      }
   where
    dropEnable   = 0x10000000
    rxdCtlEnable = 0x2000000

-- initTx
--   :: (MonadThrow m, MonadIO m, MonadReader Device m, MonadLogger m)
--   => Int
--   -> m [TxQueue]
-- initTx numTx = do
--   R.setMask R.HLREG0 crcPadEnable

--   R.set (R.TXPBSIZE 0) bufferSize
--   mapM_ (\i -> R.set (R.TXPBSIZE i) 0) [1 .. 7]

--   -- Take a minute to appreciate the name of this register:
--   R.set R.DTXMXSZRQ 0xFFFF
--   R.clearMask R.RTTDCS dcbArbiterDisable

--   txQueues <- mapM setupQueue [0 .. (numTx - 1)]
--   -- Enable Tx again.
--   R.set R.DMATXCTL txEnable
--   return txQueues
--  where
--   crcPadEnable      = 0x401
--   bufferSize        = 0xA000
--   dcbArbiterDisable = 0x40
--   txEnable          = 0x1
--   setupQueue
--     :: (MonadThrow m, MonadIO m, MonadReader Device m, MonadLogger m)
--     => Int
--     -> m TxQueue
--   setupQueue index = do
--     -- Setup descriptor ring.
--     ptr <- allocateRaw
--       (numTxQueueEntries * (sizeOf (undefined :: TransmitDescriptor) + 2048))
--       True
--     physAddr <- translate ptr
--     R.set (R.TDBAL index) $ fromIntegral (physAddr .&. 0xFFFFFFFF)
--     R.set (R.TDBAH index) $ fromIntegral (shift physAddr (-32))
--     -- Again, look at initRx to understand the seeming discrepancy here.
--     R.set (R.TDLEN index) $ fromIntegral
--       (numTxQueueEntries * sizeOf (undefined :: TransmitDescriptor))
--     $(logDebug)
--       $  "Tx Region "
--       <> show index
--       <> "at "
--       <> show ptr
--       <> " (physical="
--       <> T.pack (showHex physAddr "")
--       <> ")."
--     txdCtl <- wbMagic <$> R.get (R.TXDCTL index)

--     -- Fill the ring up with descriptors.
--     -- TODO: Really not sure if this is even necessary.
--     let ptrs =
--           [ ptr `plusPtr` (i * sizeOf (undefined :: TransmitDescriptor))
--           | i <- [0 .. (numTxQueueEntries - 1)]
--           ]
--     mapM_
--         (\(p, i) -> liftIO $ poke
--           p
--           ReadTx
--             { tdBufAddr      = fromIntegral $ (numRxQueueEntries + i) * sizeOf
--               (undefined :: TransmitDescriptor)
--             , tdCmdTypeLen   = 0
--             , tdOlInfoStatus = 0
--             }
--         )
--       $ zip ptrs [0 ..]

--     -- Set queue to empty.
--     R.set (R.TDH index) 0
--     R.set (R.TDT index) 0

--     R.setMask (R.TXDCTL index) txdCtlEnable
--     R.waitSet (R.TXDCTL index) txdCtlEnable

--     return TxQueue {_txqDescriptors = CList.fromList ptrs}
--    where
--     wbMagic      = (.|. 0x40824) . (.&. complement 0x3F3F3F)
--     txdCtlEnable = 0x2000000

waitForLink :: (MonadIO m, MonadReader Device m, MonadLogger m) => Int -> m ()
waitForLink maxTries = do
  $(logDebug) "Waiting for link..."
  waitUntil 0
 where
  waitUntil numTries | numTries == maxTries =
    $(logDebug) "Maximum wait time for link exceeded."
  waitUntil numTries = do
    speed <- linkSpeed
    case speed of
      LinkNotReady -> do
        liftIO $ usleep 10000
        waitUntil (numTries + 1)
      Link100M -> $(logDebug) "Link speed was set to 100MBit/s"
      Link1G   -> $(logDebug) "Link speed was set to 1GBit/s"
      Link10G  -> $(logDebug) "Link speed was set to 10GBit/s"

linkSpeed :: (MonadIO m, MonadReader Device m) => m LinkSpeed
linkSpeed = do
  links <- R.get R.LINKS
  if (links .&. linksUp) == 0
    then return LinkNotReady
    else
      return
        $ let l = links .&. links82599
          in  case l of
                _ | l == links100M -> Link100M
                _ | l == links1G   -> Link1G
                _ | l == links10G  -> Link10G
                _                  -> LinkNotReady
 where
  linksUp    = 0x40000000
  links82599 = 0x30000000
  links100M  = 0x10000000
  links1G    = 0x20000000
  links10G   = 0x30000000

