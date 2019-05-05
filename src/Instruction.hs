module Instruction (Instruction(..), Opcode(..), Operand(..), decodeInstruction) where

import Cpu                 (HasCpu (..))
import Memory              (Address, HasMemory (..))
import Util                (decodeWord16)

import Control.Lens        (use, (+=), (^.))
import Control.Monad.State (State, gets)
import Data.Int            (Int32, Int8)
import Data.Word           (Word8)

-- TODO: Restrict the addressing mode for each opcode to only valid modes
data RawInstruction = RawInstruction Opcode AddressingMode
    deriving (Show)
data Instruction = Instruction Opcode Operand
    deriving (Show)

data Opcode = ADC
            | AND
            | ASL
            | BCC
            | BCS
            | BEQ
            | BIT
            | BMI
            | BNE
            | BPL
            | BRK
            | BVC
            | BVS
            | CLC
            | CLD
            | CLI
            | CLV
            | CMP
            | CPX
            | CPY
            | DEC
            | DEX
            | DEY
            | EOR
            | INC
            | INX
            | INY
            | JMP
            | JSR
            | LDA
            | LDX
            | LDY
            | LSR
            | NOP
            | ORA
            | PHA
            | PHP
            | PLA
            | PLP
            | ROL
            | ROR
            | RTI
            | RTS
            | SBC
            | SEC
            | SED
            | SEI
            | STA
            | STX
            | STY
            | TAX
            | TAY
            | TSX
            | TXA
            | TXS
            | TYA
    deriving (Show)

data AddressingMode = Implicit
                    | Accumulator
                    | Immediate Word8
                    | ZeroPage Word8
                    | ZeroPageX Word8
                    | ZeroPageY Word8
                    | Relative Word8
                    | Absolute Address
                    | AbsoluteX Address
                    | AbsoluteY Address
                    | Indirect Address
                    | IndexedIndirect Word8
                    | IndirectIndexed Word8
    deriving (Show)

data Operand = NoOperand
             | DataOperand Word8
             | AddressOperand Address
    deriving (Show)

decodeInstruction :: HasCpu c => State c Instruction
decodeInstruction = do
    RawInstruction opcode mode <- fetchRawInstruction
    operand <- gets $ flip fetchOperand mode
    return $ Instruction opcode operand

fetchRawInstruction :: HasCpu c => State c RawInstruction
fetchRawInstruction = do
    pc <- use programCounter
    byte1 <- use $ memoryByte pc
    case byte1 of
        0x00 -> oneByteInstruction BRK Implicit
        0x01 -> twoByteInstruction ORA IndexedIndirect
        0x05 -> twoByteInstruction ORA ZeroPage
        0x06 -> twoByteInstruction ASL ZeroPage
        0x08 -> oneByteInstruction PHP Implicit
        0x09 -> twoByteInstruction ORA Immediate
        0x0A -> oneByteInstruction ASL Accumulator
        0x0D -> threeByteInstruction ORA Absolute
        0x0E -> threeByteInstruction ASL Absolute
        0x10 -> twoByteInstruction BPL Relative
        0x11 -> twoByteInstruction ORA IndirectIndexed
        0x15 -> twoByteInstruction ORA ZeroPageX
        0x16 -> twoByteInstruction ASL ZeroPageX
        0x18 -> oneByteInstruction CLC Implicit
        0x19 -> threeByteInstruction ORA AbsoluteY
        0x1D -> threeByteInstruction ORA AbsoluteX
        0x1E -> threeByteInstruction ASL AbsoluteX
        0x20 -> threeByteInstruction JSR Absolute
        0x21 -> twoByteInstruction AND IndexedIndirect
        0x24 -> twoByteInstruction BIT ZeroPage
        0x25 -> twoByteInstruction AND ZeroPage
        0x26 -> twoByteInstruction ROL ZeroPage
        0x28 -> oneByteInstruction PLP Implicit
        0x29 -> twoByteInstruction AND Immediate
        0x2A -> oneByteInstruction ROL Accumulator
        0x2C -> threeByteInstruction BIT Absolute
        0x2D -> threeByteInstruction AND Absolute
        0x2E -> threeByteInstruction ROL Absolute
        0x30 -> twoByteInstruction BMI Relative
        0x31 -> twoByteInstruction AND IndirectIndexed
        0x35 -> twoByteInstruction AND ZeroPageX
        0x36 -> twoByteInstruction ROL ZeroPageX
        0x38 -> oneByteInstruction SEC Implicit
        0x39 -> threeByteInstruction AND AbsoluteY
        0x3D -> threeByteInstruction AND AbsoluteX
        0x3E -> threeByteInstruction ROL AbsoluteX
        0x40 -> oneByteInstruction RTI Implicit
        0x41 -> twoByteInstruction EOR IndexedIndirect
        0x45 -> twoByteInstruction EOR ZeroPage
        0x46 -> twoByteInstruction LSR ZeroPage
        0x48 -> oneByteInstruction PHA Implicit
        0x49 -> twoByteInstruction EOR Immediate
        0x4A -> oneByteInstruction LSR Accumulator
        0x4C -> threeByteInstruction JMP Absolute
        0x4D -> threeByteInstruction EOR Absolute
        0x4E -> threeByteInstruction LSR Absolute
        0x50 -> twoByteInstruction BVC Relative
        0x51 -> twoByteInstruction EOR IndirectIndexed
        0x55 -> twoByteInstruction EOR ZeroPageX
        0x56 -> twoByteInstruction LSR ZeroPageX
        0x58 -> oneByteInstruction CLI Implicit
        0x59 -> threeByteInstruction EOR AbsoluteY
        0x5D -> threeByteInstruction EOR AbsoluteX
        0x5E -> threeByteInstruction LSR AbsoluteX
        0x60 -> oneByteInstruction RTS Implicit
        0x61 -> twoByteInstruction ADC IndexedIndirect
        0x65 -> twoByteInstruction ADC ZeroPage
        0x66 -> twoByteInstruction ROR ZeroPage
        0x68 -> oneByteInstruction PLA Implicit
        0x69 -> twoByteInstruction ADC Immediate
        0x6A -> oneByteInstruction ROR Accumulator
        0x6C -> threeByteInstruction JMP Indirect
        0x6D -> threeByteInstruction ADC Absolute
        0x6E -> threeByteInstruction ROR Absolute
        0x70 -> twoByteInstruction BVS Relative
        0x71 -> twoByteInstruction ADC IndirectIndexed
        0x75 -> twoByteInstruction ADC ZeroPageX
        0x76 -> twoByteInstruction ROR ZeroPageX
        0x78 -> oneByteInstruction SEI Implicit
        0x79 -> threeByteInstruction ADC AbsoluteY
        0x7D -> threeByteInstruction ADC AbsoluteX
        0x7E -> threeByteInstruction ROR AbsoluteX
        0x81 -> twoByteInstruction STA IndexedIndirect
        0x84 -> twoByteInstruction STY ZeroPage
        0x85 -> twoByteInstruction STA ZeroPage
        0x86 -> twoByteInstruction STX ZeroPage
        0x88 -> oneByteInstruction DEY Implicit
        0x8A -> oneByteInstruction TXA Implicit
        0x8C -> threeByteInstruction STY Absolute
        0x8D -> threeByteInstruction STA Absolute
        0x8E -> threeByteInstruction STX Absolute
        0x90 -> twoByteInstruction BCC Relative
        0x91 -> twoByteInstruction STA IndirectIndexed
        0x94 -> twoByteInstruction STY ZeroPageX
        0x95 -> twoByteInstruction STA ZeroPageX
        0x96 -> twoByteInstruction STX ZeroPageY
        0x98 -> oneByteInstruction TYA Implicit
        0x99 -> threeByteInstruction STA AbsoluteY
        0x9A -> oneByteInstruction TXS Implicit
        0x9D -> threeByteInstruction STA AbsoluteX
        0xA0 -> twoByteInstruction LDY Immediate
        0xA1 -> twoByteInstruction LDA IndexedIndirect
        0xA2 -> twoByteInstruction LDX Immediate
        0xA4 -> twoByteInstruction LDY ZeroPage
        0xA5 -> twoByteInstruction LDA ZeroPage
        0xA6 -> twoByteInstruction LDX ZeroPage
        0xA8 -> oneByteInstruction TAY Implicit
        0xA9 -> twoByteInstruction LDA Immediate
        0xAA -> oneByteInstruction TAX Implicit
        0xAC -> threeByteInstruction LDY Absolute
        0xAD -> threeByteInstruction LDA Absolute
        0xAE -> threeByteInstruction LDX Absolute
        0xB0 -> twoByteInstruction BCS Relative
        0xB1 -> twoByteInstruction LDA IndirectIndexed
        0xB4 -> twoByteInstruction LDY ZeroPageX
        0xB5 -> twoByteInstruction LDA ZeroPageX
        0xB6 -> twoByteInstruction LDX ZeroPageY
        0xB8 -> oneByteInstruction CLV Implicit
        0xB9 -> threeByteInstruction LDA AbsoluteY
        0xBA -> oneByteInstruction TSX Implicit
        0xBC -> threeByteInstruction LDY AbsoluteX
        0xBD -> threeByteInstruction LDA AbsoluteX
        0xBE -> threeByteInstruction LDX AbsoluteY
        0xC0 -> twoByteInstruction CPY Immediate
        0xC1 -> twoByteInstruction CMP IndexedIndirect
        0xC4 -> twoByteInstruction CPY ZeroPage
        0xC5 -> twoByteInstruction CMP ZeroPage
        0xC6 -> twoByteInstruction DEC ZeroPage
        0xC8 -> oneByteInstruction INY Implicit
        0xC9 -> twoByteInstruction CMP Immediate
        0xCA -> oneByteInstruction DEX Implicit
        0xCC -> threeByteInstruction CPY Absolute
        0xCD -> threeByteInstruction CMP Absolute
        0xCE -> threeByteInstruction DEC Absolute
        0xD0 -> twoByteInstruction BNE Relative
        0xD1 -> twoByteInstruction CMP IndirectIndexed
        0xD5 -> twoByteInstruction CMP ZeroPageX
        0xD6 -> twoByteInstruction DEC ZeroPageX
        0xD8 -> oneByteInstruction CLD Implicit
        0xD9 -> threeByteInstruction CMP AbsoluteY
        0xDD -> threeByteInstruction CMP AbsoluteX
        0xDE -> threeByteInstruction DEC AbsoluteX
        0xE0 -> twoByteInstruction CPX Immediate
        0xE1 -> twoByteInstruction SBC IndexedIndirect
        0xE4 -> twoByteInstruction CPX ZeroPage
        0xE5 -> twoByteInstruction SBC ZeroPage
        0xE6 -> twoByteInstruction INC ZeroPage
        0xE8 -> oneByteInstruction INX Implicit
        0xE9 -> twoByteInstruction SBC Immediate
        0xEA -> oneByteInstruction NOP Implicit
        0xEC -> threeByteInstruction CPX Absolute
        0xED -> threeByteInstruction SBC Absolute
        0xEE -> threeByteInstruction INC Absolute
        0xF0 -> twoByteInstruction BEQ Relative
        0xF1 -> twoByteInstruction SBC IndirectIndexed
        0xF5 -> twoByteInstruction SBC ZeroPageX
        0xF6 -> twoByteInstruction INC ZeroPageX
        0xF8 -> oneByteInstruction SED Implicit
        0xF9 -> threeByteInstruction SBC AbsoluteY
        0xFD -> threeByteInstruction SBC AbsoluteX
        0xFE -> threeByteInstruction INC AbsoluteX
        -- TODO: Propagate errors better and add a better error message
        _    ->  undefined
  where
    oneByteInstruction :: HasCpu c => Opcode -> AddressingMode -> State c RawInstruction
    oneByteInstruction opcode mode = do
        programCounter += 1
        return $ RawInstruction opcode mode
    twoByteInstruction :: HasCpu c => Opcode -> (Word8 -> AddressingMode) -> State c RawInstruction
    twoByteInstruction opcode mode = do
        pc <- use programCounter
        byte2 <- use $ memoryByte $ pc + 1
        programCounter += 2
        return $ RawInstruction opcode $ mode byte2
    threeByteInstruction :: HasCpu c => Opcode -> (Address -> AddressingMode) -> State c RawInstruction
    threeByteInstruction opcode mode = do
        pc <- use programCounter
        byte2 <- use $ memoryByte $ pc + 1
        byte3 <- use $ memoryByte $ pc + 2
        programCounter += 3
        return $ RawInstruction opcode $ mode $ decodeWord16 byte2 byte3

fetchOperand :: HasCpu c => c -> AddressingMode -> Operand
fetchOperand _ Implicit = NoOperand
fetchOperand cpu Accumulator = DataOperand $ cpu^.accumulator
fetchOperand _ (Immediate imm) = DataOperand imm
fetchOperand _ (ZeroPage address) = AddressOperand $ fromIntegral address
fetchOperand cpu (ZeroPageX address) =
    AddressOperand $ fromIntegral $ address + cpu^.indexX
fetchOperand cpu (ZeroPageY address) =
    AddressOperand $ fromIntegral $ address + cpu^.indexY
fetchOperand cpu (Relative rel) = AddressOperand $ fromIntegral $ offset + pc
  where
    offset = fromIntegral (fromIntegral rel :: Int8) :: Int32
    pc = (fromIntegral $ cpu^.programCounter) :: Int32
fetchOperand _ (Absolute address) = AddressOperand address
fetchOperand cpu (AbsoluteX address) =
    AddressOperand $ address + fromIntegral (cpu^.indexX)
fetchOperand cpu (AbsoluteY address) =
    AddressOperand $ address + fromIntegral (cpu^.indexY)
fetchOperand cpu (Indirect indirect) =
    AddressOperand $ decodeWord16 lowByte highByte
  where
    lowByte = cpu^.memoryByte (fromIntegral indirect)
    highByte = cpu^.memoryByte (fromIntegral $ indirect + 1)
fetchOperand cpu (IndexedIndirect indirect) = fetchOperand cpu $ ZeroPage $ indirect + fromIntegral (cpu^.indexX)
fetchOperand cpu (IndirectIndexed indirect) =
    AddressOperand $ address + fromIntegral (cpu^.indexY)
  where
    (AddressOperand address) = fetchOperand cpu (ZeroPage indirect)
