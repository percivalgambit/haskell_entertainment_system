{-# LANGUAGE Rank2Types      #-}
{-# LANGUAGE TemplateHaskell #-}

module Emulator (newEmulator) where

import           Cpu             (HasCpu (..), HasFlags (..))
import           Mapper          (Mapper (..), runMapper)
import           Memory          (HasMemory (..))
import           MemoryBank      (BankAddress (..), BankType (..), ChrRomData,
                                  ChrRomData, PrgRomData, unChrRomData,
                                  unPrgRomData)
import           NesFile         (NesFile (..), fileChrRom, filePrgRom,
                                  parseNesFile)

import           Control.Lens    (Lens', ix, lens, makeLenses, (&), (.~), (^.))
import           Data.Bits       (bit, testBit, zeroBits, (.|.))
import qualified Data.ByteString as B
import           Data.Word       (Word16, Word8)

data Flags = Flags
    { _emulatorCarry            :: Bool
    , _emulatorZero             :: Bool
    , _emulatorInterruptDisable :: Bool
    , _emulatorDecimal          :: Bool
    , _emulatorBreakCommand     :: Bool
    , _emulatorOverflow         :: Bool
    , _emulatorNegative         :: Bool
    } deriving (Show)
makeLenses ''Flags

data Memory = Memory
    { _mainRom :: B.ByteString
    , _mainRam :: B.ByteString
    , _prgRom  :: PrgRomData
    , _prgRam  :: B.ByteString
    , _chrRom  :: ChrRomData
    , _chrRam  :: B.ByteString
    , _mapper  :: Mapper
    } deriving (Show)
makeLenses ''Memory

data Cpu = Cpu
    { _emulatorProgramCounter :: Word16
    , _emulatorStackPointer   :: Word8
    , _emulatorAccumulator    :: Word8
    , _emulatorIndexX         :: Word8
    , _emulatorIndexY         :: Word8
    , _emulatorStatus         :: Flags
    } deriving (Show)
makeLenses ''Cpu

data Emulator = Emulator
    { _cpu    :: Cpu
    , _memory :: Memory
    } deriving (Show)
makeLenses ''Emulator

instance HasMemory Emulator where
    memoryByte n = lens get set
      where
        memoryBank :: BankType -> Lens' Memory B.ByteString
        memoryBank MainRom = mainRom
        memoryBank MainRam = mainRam
        memoryBank PrgRom  = prgRom . unPrgRomData
        memoryBank PrgRam  = prgRam
        memoryBank ChrRom  = chrRom . unChrRomData
        memoryBank ChrRam  = chrRam
        get :: Emulator -> Word8
        get e = let (BankAddress bank n') = runMapper (e^.memory.mapper) n
                in e^.memory . memoryBank bank & flip B.index (fromIntegral n')
        set :: Emulator -> Word8 -> Emulator
        set e val = let (BankAddress bank n') = runMapper (e^.memory.mapper) n
                    in e & memory . memoryBank bank . ix (fromIntegral n') .~ val

instance HasFlags Emulator where
    flagByte = lens get set
      where
        get e =
            flagToBit e carry 0 .|.
            flagToBit e zero 1 .|.
            flagToBit e interruptDisable 2 .|.
            flagToBit e decimal 3 .|.
            flagToBit e overflow 6 .|.
            flagToBit e negative 7
        flagToBit e getFlag bitPos =
            if e^.getFlag
            then bit bitPos
            else zeroBits
        set e val =
            e & cpu.emulatorStatus .~
                Flags { _emulatorCarry = testBit val 0
                      , _emulatorZero = testBit val 1
                      , _emulatorInterruptDisable = testBit val 2
                      , _emulatorDecimal = testBit val 3
                      , _emulatorBreakCommand = False
                      , _emulatorOverflow = testBit val 6
                      , _emulatorNegative = testBit val 7
                }
    carry = cpu.emulatorStatus.emulatorCarry
    zero = cpu.emulatorStatus.emulatorZero
    interruptDisable = cpu.emulatorStatus.emulatorInterruptDisable
    decimal = cpu.emulatorStatus.emulatorDecimal
    breakCommand = cpu.emulatorStatus.emulatorBreakCommand
    overflow = cpu.emulatorStatus.emulatorOverflow
    negative = cpu.emulatorStatus.emulatorNegative

instance HasCpu Emulator where
    programCounter = cpu.emulatorProgramCounter
    stackPointer = cpu.emulatorStackPointer
    accumulator = cpu.emulatorAccumulator
    indexX = cpu.emulatorIndexX
    indexY = cpu.emulatorIndexY

newEmulator :: B.ByteString -> Emulator
newEmulator bs = Emulator
    { _cpu = Cpu
        { _emulatorProgramCounter = 0
        , _emulatorStackPointer = 0
        , _emulatorAccumulator = 0
        , _emulatorIndexX = 0
        , _emulatorIndexY = 0
        , _emulatorStatus = Flags
            { _emulatorCarry = False
            , _emulatorZero = False
            , _emulatorInterruptDisable = False
            , _emulatorDecimal = False
            , _emulatorBreakCommand = False
            , _emulatorOverflow = False
            , _emulatorNegative = False
            }
        }
    , _memory = Memory
        { _mainRom = B.empty
        , _mainRam = B.empty
        , _prgRom = nesFile^.filePrgRom
        , _prgRam = B.empty
        , _chrRom = nesFile^.fileChrRom
        , _chrRam = B.empty
        , _mapper = Mapper0
        }
    }
  where
    nesFile = parseNesFile bs
