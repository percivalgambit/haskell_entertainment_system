{-# LANGUAGE RankNTypes #-}

module CPU
  ( CPU,
    StatusFlag,
    StatusBits (..),
    newCpu,
    carryFlag,
    zeroFlag,
    interruptFlag,
    decimalFlag,
    overflowFlag,
    negativeFlag,
    showRegisters,
  )
where

import qualified Control.Lens as L
import Control.Monad (guard)
import qualified Data.Bits as B
import qualified Data.Bits.Lens as B
import Data.Function ((&))
import Data.Generics.Labels ()
import Data.List (intercalate)
import Data.Word (Word16, Word8)
import GHC.Generics (Generic)
import Text.Printf (printf)

carryIx, zeroIx, interruptIx, decimalIx, overflowIx, negativeIx :: Int
carryIx = 0
zeroIx = 1
interruptIx = 2
decimalIx = 3
overflowIx = 6
negativeIx = 7

newtype StatusBits = StatusBits {toWord :: Word8}
  deriving (Generic)

instance Show StatusBits where
  show :: StatusBits -> String
  show (StatusBits s) =
    L.iconcatMapOf B.bits flagToString s & intercalate ","
    where
      flagToString i = (ixString i <$) . guard
      ixString i
        | i == carryIx = "carry"
        | i == zeroIx = "zero"
        | i == interruptIx = "interrupt"
        | i == decimalIx = "decimal"
        | i == overflowIx = "overflow"
        | i == negativeIx = "negative"
        | otherwise = ""

data CPU = CPU
  { pc :: Word16,
    sp :: Word8,
    a :: Word8,
    x :: Word8,
    y :: Word8,
    status :: StatusBits
  }
  deriving (Show, Generic)

newCpu :: Word16 -> CPU
newCpu gameCodeAddress =
  CPU
    { pc = gameCodeAddress,
      sp = 0xFD,
      a = 0,
      x = 0,
      y = 0,
      status = StatusBits $ B.bit interruptIx
    }

type StatusFlag = L.Lens' CPU Bool

statusFlag :: Int -> StatusFlag
statusFlag i = #status . #toWord . B.bitAt i

carryFlag :: StatusFlag
carryFlag = statusFlag carryIx

zeroFlag :: StatusFlag
zeroFlag = statusFlag zeroIx

interruptFlag :: StatusFlag
interruptFlag = statusFlag interruptIx

decimalFlag :: StatusFlag
decimalFlag = statusFlag decimalIx

overflowFlag :: StatusFlag
overflowFlag = statusFlag overflowIx

negativeFlag :: StatusFlag
negativeFlag = statusFlag negativeIx

showRegisters :: CPU -> String
showRegisters (CPU {a, x, y, status = (StatusBits status), sp}) =
  printf "A:%02X X:%02X Y:%02X P:%02X SP:%02X" a x y status sp