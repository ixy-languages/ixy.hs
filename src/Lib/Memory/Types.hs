module Lib.Memory.Types
    ( Translation(..)
    , MemPool(..)
    , PacketBuf(..)
    ) where

import Lib.Prelude

import qualified Data.ByteString as B
import Foreign.Ptr (castPtr)
import Foreign.Storable (Storable, alignment, peek, peekByteOff, poke, pokeByteOff, sizeOf)

data Translation = Translation
    { trPhysical :: Word
    , trVirtual :: Ptr Word
    }

data MemPool = MemPool
    { mpBase :: Ptr PacketBuf
    , mpBufSize :: Word
    , mpTop :: Int
    , mpFreeBufs :: [Int]
    } deriving (Show)

data PacketBuf = PacketBuf
    { pbPhysical :: !Word
    , pbMemPoolIndex :: Int
    , pbBufSize :: Word
    , pbBuf :: ByteString
    } deriving (Show)

instance Storable PacketBuf where
    sizeOf _ = 2048
    alignment _ = 2048
    peek ptr = do
        physAddr <- peek (castPtr ptr :: Ptr Word)
        mpIndex <- peekByteOff (castPtr ptr :: Ptr Int) (sizeOf physAddr)
        bufSize <- peekByteOff (castPtr ptr :: Ptr Word) (sizeOf physAddr + sizeOf mpIndex)
        let indices =
                if bufSize /= 0
                    then [0 .. bufSize]
                    else []
        bufWord <- mapM (peekWord ptr (sizeOf physAddr + sizeOf mpIndex + sizeOf bufSize)) indices
        return PacketBuf {pbPhysical = physAddr, pbMemPoolIndex = mpIndex, pbBufSize = fromIntegral bufSize, pbBuf = B.pack bufWord}
      where
        peekWord wPtr offset i = peekByteOff (castPtr wPtr :: Ptr Word8) (offset + i * sizeOf (0 :: Word8))
    poke ptr packetBuf = do
        poke (castPtr ptr :: Ptr Word) phys
        pokeByteOff (castPtr ptr :: Ptr Int) (sizeOf phys) index
        pokeByteOff (castPtr ptr :: Ptr Word) (sizeOf phys + sizeOf index) size
        mapM_ (pokeWord ptr (sizeOf phys + sizeOf index + sizeOf size)) $ zip [0 .. (fromIntegral size)] buf
      where
        phys = pbPhysical packetBuf
        index = pbMemPoolIndex packetBuf
        size = pbBufSize packetBuf
        buf = B.unpack $ pbBuf packetBuf
        pokeWord wPtr offset (i, w) = pokeByteOff wPtr (offset + i * sizeOf w) w
