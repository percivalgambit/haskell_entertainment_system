{-# LANGUAGE RecordWildCards #-}

module Rom (Rom (..), Mirroring (..), newRom) where

import Bits (maskValue)
import Control.Arrow ((>>>))
import Control.Lens.Getter (to, (^.))
import Control.Monad (replicateM, replicateM_, unless, when)
import Data.Binary.Get (Get, getByteString, getWord8, runGet)
import Data.Bits ((.&.))
import qualified Data.Bits.Lens as B
import qualified Data.ByteString as BS
import Data.Char (chr)
import Data.Functor ((<&>))
import Data.Word (Word8)

data Mirroring = VERTICAL | HORIZONTAL | FOUR_SCREEN deriving (Show)

data Rom = Rom
  { prgRom :: BS.ByteString,
    chrRom :: BS.ByteString,
    mapper :: Word8,
    mirroring :: Mirroring
  }
  deriving (Show)

newRom :: FilePath -> IO Rom
newRom filePath = BS.readFile filePath >>= (BS.fromStrict >>> runGet getRom >>> return)

prgRomPageSize :: Int
prgRomPageSize = 16384

chrRomPageSize :: Int
chrRomPageSize = 8192

readBytes :: Int -> Get [Word8]
readBytes = flip replicateM getWord8

skipBytes :: Int -> Get ()
skipBytes = flip replicateM_ getWord8

getRom :: Get Rom
getRom = do
  getMagicTag
  prgRomSize <- getWord8 <&> fromIntegral
  chrRomSize <- getWord8 <&> fromIntegral
  HeaderFlags {..} <- getHeaderFlags
  case version of
    INES -> skipBytes 5
    NES2 -> fail "NES2.0 format is not supported"
    _ -> fail "Unknown iNES version"
  when hasTrainer (skipBytes 512)
  prgRom <- getByteString (prgRomSize * prgRomPageSize)
  chrRom <- getByteString (chrRomSize * chrRomPageSize)
  let mirroring = case (fourScreenNametable, verticalMirroring) of
        (True, _) -> FOUR_SCREEN
        (False, True) -> VERTICAL
        (False, False) -> HORIZONTAL
  return Rom {..}

getMagicTag :: Get ()
getMagicTag = do
  let getChar = getWord8 <&> (fromIntegral >>> chr)
  magicTag <- replicateM 4 getChar
  unless (magicTag == "NES\x1A") (fail "File is not in iNES format")

data Version = INES | NES2 | UNKNOWN_VERSION deriving (Show)

data HeaderFlags = HeaderFlags
  { hasTrainer :: Bool,
    verticalMirroring :: Bool,
    fourScreenNametable :: Bool,
    mapper :: Word8,
    version :: Version
  }
  deriving (Show)

getHeaderFlags :: Get HeaderFlags
getHeaderFlags = do
  [flags0, flags1, flags2, flags3, flags4] <- readBytes 5
  return
    HeaderFlags
      { hasTrainer = flags0 ^. B.bitAt 2,
        verticalMirroring = flags0 ^. B.bitAt 0 . to not,
        fourScreenNametable = flags0 ^. B.bitAt 3 . to not,
        mapper = (flags1 .&. 0b11110000) .&. maskValue 0b11110000 flags0,
        version = case maskValue 0b1100 flags1 of
          0 -> INES
          2 -> NES2
          _ -> UNKNOWN_VERSION
      }