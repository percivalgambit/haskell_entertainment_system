{-# LANGUAGE RankNTypes #-}

module Memory (Memory, newMemory, stackPage, gameCodeAddress, memoryIx, readWord16, memoryRange) where

import Bits (packWord16)
import qualified Control.Lens as L
import Control.Lens.Operators ((&), (.~))
import qualified Control.Lens.Unsound as L (lensProduct)
import qualified Data.ByteString as BS
import Data.Word (Word16, Word8)
import Numeric (showHex)
import Types (Address)

stackPage :: Word8
stackPage = 0x01

gameCodeAddress :: Address
gameCodeAddress = 0x0600

newtype Memory = Memory BS.ByteString deriving (Show)

newMemory :: Memory
newMemory = Memory $ BS.pack (replicate 0x07FF 0)

memoryIx :: Address -> L.IndexedLens' Address Memory Word8
memoryIx addr = L.ilens getter setter
  where
    getter (Memory mem) = (addr, mem `BS.index` addrInt)
    setter (Memory mem) val = Memory $ mem & L.ix addrInt .~ val
    addrInt = fromIntegral addr

readWord16 :: Address -> L.Getter Memory Word16
readWord16 addr = L.lensProduct (memoryIx addr) (memoryIx $ addr + 1) . packWord16

memoryRange :: (Address, Address) -> L.Lens' Memory BS.ByteString
memoryRange (startAddr, endAddr) = L.lens getter setter
  where
    getter (Memory mem) = mem & BS.drop startAddrInt & BS.take sizeInt
    setter (Memory mem) data' =
      if BS.length data' /= fromIntegral (endAddr - startAddr)
        then
          error $
            "Size mismatch: trying to write data of size "
              ++ show (BS.length data')
              ++ " to memory region from (0x"
              ++ showHex startAddr "-0x"
              ++ showHex endAddr "), size "
              ++ show sizeInt
        else
          let firstPart = BS.take startAddrInt mem
              lastPart = BS.drop endAddrInt mem
           in Memory $ BS.concat [firstPart, data', lastPart]
    startAddrInt = fromIntegral startAddr
    endAddrInt = fromIntegral endAddr
    sizeInt = fromIntegral $ endAddr - startAddr