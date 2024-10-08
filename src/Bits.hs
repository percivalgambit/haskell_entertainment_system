module Bits (toWord16, packWord16, fromBool) where

import qualified Control.Lens as L
import Control.Lens.Operators ((&), (.~))
import qualified Control.Lens.Unsound as L (lensProduct)
import qualified Data.Bits.Lens as B
import Data.Word (Word16, Word8)

toWord16 :: (Integral a) => a -> Word16
toWord16 = fromIntegral

packWord16 :: L.Iso' (Word8, Word8) Word16
packWord16 = L.iso forward backward
  where
    forward (lo, hi) = 0 & B.byteAt 0 .~ lo & B.byteAt 1 .~ hi
    backward = L.view (L.lensProduct (B.byteAt 0) (B.byteAt 1))

fromBool :: (Integral b) => Bool -> b
fromBool True = 1
fromBool False = 0