{-# LANGUAGE BinaryLiterals  #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}

module ParseNesFile (parseNesFile, NesFile(..), PrgConfig(..), ChrConfig(..), PpuConfig(..), PpuTiming, ConsoleType, Mirroring, FileFormat) where

import           BitGet          (BitGet, getAsWord16, getAsWord32, getAsWord8,
                                  getBit, getByteString, isEmpty, lookAhead,
                                  remaining, runBitGet, skip)

import           Control.Lens    (makeLenses, (&), (.~), (?~), (^.))
import           Control.Monad   (unless)
import           Data.Bits       (shiftL)
import           Data.Bits.Lens  ((.|.~))
import           Data.Bool       (bool)
import qualified Data.ByteString as B
import           Data.Word       (Word16, Word8)

data FileFormat = Nes2 | INes | ArchaicINes deriving (Show)

data Mirroring = HorizontalMirroring
               | VerticalMirroring
               | FourScreenMirroring
    deriving (Show)

data ConsoleType = NesConsole
                 | VsConsole
                 | Playchoice10Console
    deriving (Show)

data PpuTiming = NTSCTiming -- RP2C02
               | PALTiming -- RP2C07
    deriving (Show)

data PrgConfig = PrgConfig
    { _prgRomSize :: Word16 -- in 16 KB units
    , _prgRamSize :: Maybe Word8
    } deriving (Show)
makeLenses ''PrgConfig

newtype ChrConfig = ChrConfig
    { _chrRomSize :: Word16 -- in 8 KB units
    } deriving (Show)
makeLenses ''ChrConfig

data PpuConfig = PpuConfig
    { _ppuMirroring :: Mirroring
    , _ppuTiming    :: PpuTiming
    } deriving (Show)
makeLenses ''PpuConfig

data Header = Header
    { _prgConfig    :: PrgConfig
    , _chrConfig    :: ChrConfig
    , _ppuConfig    :: PpuConfig
    , _hasTrainer   :: Bool
    , _mapperNumber :: Word16
    , _consoleType  :: ConsoleType
    } deriving (Show)
makeLenses ''Header

-- TODO: implement Trainer
data Trainer = Trainer deriving (Show)

newtype PrgRom = PrgRom B.ByteString deriving (Show)
newtype ChrRom = ChrRom B.ByteString deriving (Show)

data NesFile = NesFile
    { header  :: Header
    , trainer :: Maybe Trainer
    , prgRom  :: PrgRom
    , chrRom  :: ChrRom
    , title   :: Maybe B.ByteString
    } deriving (Show)

parseNesFile :: B.ByteString -> NesFile
parseNesFile = flip runBitGet getNesFile

getNesFile :: BitGet NesFile
getNesFile = do
    header' <- getHeader
    trainer' <- bool (return Nothing) (Just <$> getTrainer) $ header'^.hasTrainer
    prgRom' <- PrgRom <$> getByteString (8 * 16 * 1024 * (header'^.prgConfig.prgRomSize & fromIntegral))
    chrRom' <- ChrRom <$> getByteString (8 * 8 * 1024 * (header'^.chrConfig.chrRomSize & fromIntegral))
    -- TODO: Parse PlayChoice ROM data
    bitsLeft <- remaining
    title' <- if bitsLeft == 128 * 8 || bitsLeft == 127 * 8
              then Just <$> getByteString bitsLeft
              else return Nothing
    finished <- isEmpty
    if finished
        then return $ NesFile { header = header'
                              , trainer = trainer'
                              , prgRom = prgRom'
                              , chrRom = chrRom'
                              , title = title'
                              }
        else fail "Extra bytes found at end of file"

-- TODO: Implement getTrainer
getTrainer :: BitGet Trainer
getTrainer = undefined

getHeader :: BitGet Header
getHeader = do
    checkInitialHeaderConstant
    prgRomSize' <- getAsWord16 8
    chrRomSize' <- getAsWord16 8
    hasVerticalMirroring <- getBit
    prgRamSize' <- bool Nothing (Just 0) <$> getBit
    hasTrainer' <- getBit
    hasFourScreenMirroring <- getBit
    let mirroring | hasFourScreenMirroring = FourScreenMirroring
                  | hasVerticalMirroring = VerticalMirroring
                  | otherwise = HorizontalMirroring
    mapperLowerNybble <- getAsWord16 4
    let header' = Header { _prgConfig = PrgConfig { _prgRomSize = prgRomSize'
                                                  , _prgRamSize = prgRamSize'
                                                  }
                         , _chrConfig = ChrConfig { _chrRomSize = chrRomSize' }
                         , _ppuConfig = PpuConfig { _ppuMirroring = mirroring
                                                  , _ppuTiming = PALTiming
                                                  }
                         , _hasTrainer = hasTrainer'
                         , _mapperNumber = mapperLowerNybble
                         , _consoleType = NesConsole
                         }
    format <- lookAhead $ skip 2 >> getFileFormat
    case format of
        ArchaicINes -> skip 72 >> return header'
        INes        -> finishParsingINes header'
        Nes2        -> finishParsingNes2 header'

checkInitialHeaderConstant :: BitGet ()
checkInitialHeaderConstant = do
    let headerConstant = B.pack [0x4E, 0x45, 0x53, 0x1A] -- "NES\EOF"
    initial <- getByteString 32
    unless (initial == headerConstant) $ fail "Filetype is not iNES"

getFileFormat :: BitGet FileFormat
getFileFormat = do
    formatSpecifier <- getAsWord8 2
    skip 36
    headerEnd <- getAsWord32 32
    let fileFormat | formatSpecifier == 0b10 = Nes2
                   | formatSpecifier == 0b00 && headerEnd == 0 = INes
                   | otherwise = ArchaicINes
    return fileFormat

finishParsingINes :: Header -> BitGet Header
finishParsingINes partialHeader = do
    consoleType' <- getAsWord8 2 >>=
        \case 0b00 -> return NesConsole
              0b01 -> return VsConsole
              0b10 -> return Playchoice10Console
              _   -> fail "Unknown console type"
    skip 2
    mapperUpperNybble <- getAsWord16 4
    prgRamSize' <- getAsWord8 8
    ppuTiming' <- bool NTSCTiming PALTiming <$> getBit
    skip 55
    return $ partialHeader & prgConfig.prgRamSize ?~ prgRamSize'
                           & ppuConfig.ppuTiming .~ ppuTiming'
                           & mapperNumber .|.~ mapperUpperNybble `shiftL` 4
                           & consoleType .~ consoleType'

-- TODO: implement NES 2.0 parsing
finishParsingNes2 :: Header -> BitGet Header
finishParsingNes2 = undefined
