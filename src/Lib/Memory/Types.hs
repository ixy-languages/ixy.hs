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
    { mpBase :: Ptr Word
    , mpBufSize :: Word
    , mpTop :: Int
    } deriving (Show)

instance Storable MemPool where
    sizeOf _ = sizeOf (undefined :: Ptr Word) + sizeOf (undefined :: Word) + sizeOf (undefined :: Int)
    alignment = sizeOf
    peek ptr = do
        base <- peek (castPtr ptr :: Ptr (Ptr Word))
        bufSize <- peekByteOff (castPtr ptr :: Ptr Word) (sizeOf base)
        top <- peekByteOff (castPtr ptr :: Ptr Int) (sizeOf base + sizeOf bufSize)
        return MemPool {mpBase = base, mpBufSize = bufSize, mpTop = top}
    poke ptr memPool = do
        poke (castPtr ptr :: Ptr (Ptr Word)) base
        pokeByteOff (castPtr ptr :: Ptr Word) (sizeOf base) bufSize
        pokeByteOff (castPtr ptr :: Ptr Int) (sizeOf base + sizeOf bufSize) top
      where
        base = mpBase memPool
        bufSize = mpBufSize memPool
        top = mpTop memPool

data PacketBuf = PacketBuf
    { pbPhysical :: Word
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
        peekWord ptr offset i = peekByteOff (castPtr ptr :: Ptr Word8) (offset + i * sizeOf (0 :: Word8))
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
        pokeWord ptr offset (i, w) = pokeByteOff ptr (offset + i * sizeOf w) w
