{-# LANGUAGE RankNTypes #-}

module Memory (Memory, newMemory, stackPage, interruptAddress, memoryIx, readWord16, memoryRange) where

import Bits (packWord16)
import qualified Control.Lens as L
import Control.Lens.Operators ((&), (.~))
import qualified Control.Lens.Unsound as L (lensProduct)
import Data.Bits ((.&.))
import qualified Data.ByteString as BS
import Data.Ix (inRange)
import Data.Word (Word16, Word8)
import Numeric (showHex)
import Rom (Rom (..))
import Types (Address)

stackPage :: Word8
stackPage = 0x01

interruptAddress :: Address
interruptAddress = 0xFFFE

data Memory = Memory
  { internalRam :: BS.ByteString,
    prgRom :: BS.ByteString
  }
  deriving (Show)

newMemory :: Rom -> Memory
newMemory (Rom {prgRom}) =
  Memory
    { internalRam =
        BS.pack (replicate 0x07FF 0),
      prgRom
    }

memoryIx :: Address -> L.IndexedLens' Address Memory Word8
memoryIx addr = L.ilens getter setter
  where
    inRange = flip Data.Ix.inRange (fromIntegral addr)
    getter (Memory {internalRam, prgRom}) =
      ( addr,
        if
          | inRange (0x0000, 0x1FFF) ->
              internalRam `BS.index` fromIntegral (addr .&. 0x07FF)
          | inRange (0x8000, 0xFFFF) ->
              prgRom `BS.index` (fromIntegral addr `mod` BS.length prgRom)
          | otherwise -> error $ "Memory read at address 0x" ++ showHex addr " unsupported"
      )
    setter mem@(Memory {internalRam}) val
      | inRange (0x0000, 0x1FFF) =
          mem {internalRam = internalRam & L.ix (fromIntegral $ addr .&. 0x07FF) .~ val}
      | inRange (0x8000, 0xFFFF) =
          error $ "Attempt to write cartridge rom space at address 0x" ++ showHex addr ""
      | otherwise = error $ "Memory write at address 0x" ++ showHex addr " unsupported"

readWord16 :: Address -> L.Getter Memory Word16
readWord16 addr = L.lensProduct (memoryIx addr) (memoryIx $ addr + 1) . packWord16

-- TODO: make function work with other types of memory besides internal ram
memoryRange :: (Address, Address) -> L.Lens' Memory BS.ByteString
memoryRange (startAddr, endAddr) = L.lens getter setter
  where
    getter (Memory {internalRam}) = internalRam & BS.drop startAddrInt & BS.take sizeInt
    setter (Memory {internalRam, prgRom}) data' =
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
          let firstPart = BS.take startAddrInt internalRam
              lastPart = BS.drop endAddrInt internalRam
           in Memory
                { internalRam = BS.concat [firstPart, data', lastPart],
                  prgRom
                }
    startAddrInt = fromIntegral startAddr
    endAddrInt = fromIntegral endAddr
    sizeInt = fromIntegral $ endAddr - startAddr