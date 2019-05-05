module Util (decodeWord16, decodeWord32, getHighWord8, getLowWord8) where

import Control.Category ((>>>))
import Data.Bits        (shiftL, shiftR, (.|.))
import Data.Word        (Word16, Word32, Word8)

decodeWord16 :: Word8 -> Word8 -> Word16
decodeWord16 highByte lowByte =
    (fromIntegral highByte `shiftL` 8) .|. fromIntegral lowByte

decodeWord32 :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
decodeWord32 byte1 byte2 byte3 byte4 =
    (fromIntegral byte1 `shiftL` 24) .|.
    (fromIntegral byte2 `shiftL` 16) .|.
    (fromIntegral byte3 `shiftL` 8) .|.
    fromIntegral byte4

getHighWord8 :: Word16 -> Word8
getHighWord8 = (`shiftR` 8) >>> fromIntegral

getLowWord8 :: Word16 -> Word8
getLowWord8 = fromIntegral
