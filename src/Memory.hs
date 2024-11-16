{-# LANGUAGE RankNTypes #-}

module Memory (Memory, newMemory, stackPage, gameCodeAddress, interruptAddress, memoryIx, readWord16, memoryRange) where

import Bits (packWord16)
import qualified Control.Lens as L
import Control.Lens.Operators ((&), (.~))
import qualified Control.Lens.Unsound as L (lensProduct)
import Data.Bits ((.&.))
import qualified Data.ByteString as BS
import Data.Ix (inRange)
import Data.Word (Word16, Word8)
import Numeric (showHex)
import Types (Address)

stackPage :: Word8
stackPage = 0x01

gameCodeAddress :: Address
gameCodeAddress = 0x0600

interruptAddress :: Address
interruptAddress = 0xFFFE

newtype Memory = Memory BS.ByteString deriving (Show)

newMemory :: Memory
newMemory = Memory $ BS.pack (replicate 0x07FF 0)

directMemoryIx :: Address -> L.IndexedLens' Address Memory Word8
directMemoryIx addr = L.ilens getter setter
  where
    getter (Memory mem) = (addr, mem `BS.index` addrInt)
    setter (Memory mem) val = Memory $ mem & L.ix addrInt .~ val
    addrInt = fromIntegral addr

memoryIx :: Address -> L.IndexedLens' Address Memory Word8
memoryIx addr
  | inRange (0x0000, 0x1FFF) addrInt = directMemoryIx (addr .&. 0x07FF)
  | otherwise = error $ "Memory access at address 0x" ++ showHex addr " unsupported"
  where
    addrInt = fromIntegral addr

readWord16 :: Address -> L.Getter Memory Word16
readWord16 addr = L.lensProduct (memoryIx addr) (memoryIx $ addr + 1) . packWord16

memoryRange :: (Address, Address) -> L.Lens' Memory BS.ByteString
memoryRange (startAddr, endAddr) = L.lens getter setter
  where
    getter (Memory mem) = mem & BS.drop startAddrInt & BS.take sizeInt
    setter (Memory mem) data' =
      if BS.length data' /= sizeInt
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