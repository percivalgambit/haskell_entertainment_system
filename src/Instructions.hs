module Instructions (Opcode (..), AddressingMode (..), InstructionHeader (..), Instruction (..), Operand (..), decodeInstruction) where

import Data.Word (Word8)
import Numeric (showHex)
import Types (Address)

data Opcode
  = ADC
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

data AddressingMode
  = Implicit
  | Accumulator
  | Immediate
  | ZeroPage
  | ZeroPageX
  | ZeroPageY
  | Relative
  | Absolute
  | AbsoluteX
  | AbsoluteY
  | Indirect
  | IndexedIndirect
  | IndirectIndexed
  deriving (Show)

data InstructionHeader = InstructionHeader Opcode AddressingMode deriving (Show)

decodeInstruction :: Word8 -> InstructionHeader
decodeInstruction 0x00 = InstructionHeader BRK Implicit
decodeInstruction 0x01 = InstructionHeader ORA IndexedIndirect
decodeInstruction 0x05 = InstructionHeader ORA ZeroPage
decodeInstruction 0x06 = InstructionHeader ASL ZeroPage
decodeInstruction 0x08 = InstructionHeader PHP Implicit
decodeInstruction 0x09 = InstructionHeader ORA Immediate
decodeInstruction 0x0A = InstructionHeader ASL Accumulator
decodeInstruction 0x0D = InstructionHeader ORA Absolute
decodeInstruction 0x0E = InstructionHeader ASL Absolute
decodeInstruction 0x10 = InstructionHeader BPL Relative
decodeInstruction 0x11 = InstructionHeader ORA IndirectIndexed
decodeInstruction 0x15 = InstructionHeader ORA ZeroPageX
decodeInstruction 0x16 = InstructionHeader ASL ZeroPageX
decodeInstruction 0x18 = InstructionHeader CLC Implicit
decodeInstruction 0x19 = InstructionHeader ORA AbsoluteY
decodeInstruction 0x1D = InstructionHeader ORA AbsoluteX
decodeInstruction 0x1E = InstructionHeader ASL AbsoluteX
decodeInstruction 0x20 = InstructionHeader JSR Absolute
decodeInstruction 0x21 = InstructionHeader AND IndexedIndirect
decodeInstruction 0x24 = InstructionHeader BIT ZeroPage
decodeInstruction 0x25 = InstructionHeader AND ZeroPage
decodeInstruction 0x26 = InstructionHeader ROL ZeroPage
decodeInstruction 0x28 = InstructionHeader PLP Implicit
decodeInstruction 0x29 = InstructionHeader AND Immediate
decodeInstruction 0x2A = InstructionHeader ROL Accumulator
decodeInstruction 0x2C = InstructionHeader BIT Absolute
decodeInstruction 0x2D = InstructionHeader AND Absolute
decodeInstruction 0x2E = InstructionHeader ROL Absolute
decodeInstruction 0x30 = InstructionHeader BMI Relative
decodeInstruction 0x31 = InstructionHeader AND IndirectIndexed
decodeInstruction 0x35 = InstructionHeader AND ZeroPageX
decodeInstruction 0x36 = InstructionHeader ROL ZeroPageX
decodeInstruction 0x38 = InstructionHeader SEC Implicit
decodeInstruction 0x39 = InstructionHeader AND AbsoluteY
decodeInstruction 0x3D = InstructionHeader AND AbsoluteX
decodeInstruction 0x3E = InstructionHeader ROL AbsoluteX
decodeInstruction 0x40 = InstructionHeader RTI Implicit
decodeInstruction 0x41 = InstructionHeader EOR IndexedIndirect
decodeInstruction 0x45 = InstructionHeader EOR ZeroPage
decodeInstruction 0x46 = InstructionHeader LSR ZeroPage
decodeInstruction 0x48 = InstructionHeader PHA Implicit
decodeInstruction 0x49 = InstructionHeader EOR Immediate
decodeInstruction 0x4A = InstructionHeader LSR Accumulator
decodeInstruction 0x4C = InstructionHeader JMP Absolute
decodeInstruction 0x4D = InstructionHeader EOR Absolute
decodeInstruction 0x4E = InstructionHeader LSR Absolute
decodeInstruction 0x50 = InstructionHeader BVC Relative
decodeInstruction 0x51 = InstructionHeader EOR IndirectIndexed
decodeInstruction 0x55 = InstructionHeader EOR ZeroPageX
decodeInstruction 0x56 = InstructionHeader LSR ZeroPageX
decodeInstruction 0x58 = InstructionHeader CLI Implicit
decodeInstruction 0x59 = InstructionHeader EOR AbsoluteY
decodeInstruction 0x5D = InstructionHeader EOR AbsoluteX
decodeInstruction 0x5E = InstructionHeader LSR AbsoluteX
decodeInstruction 0x60 = InstructionHeader RTS Implicit
decodeInstruction 0x61 = InstructionHeader ADC IndexedIndirect
decodeInstruction 0x65 = InstructionHeader ADC ZeroPage
decodeInstruction 0x66 = InstructionHeader ROR ZeroPage
decodeInstruction 0x68 = InstructionHeader PLA Implicit
decodeInstruction 0x69 = InstructionHeader ADC Immediate
decodeInstruction 0x6A = InstructionHeader ROR Accumulator
decodeInstruction 0x6C = InstructionHeader JMP Indirect
decodeInstruction 0x6D = InstructionHeader ADC Absolute
decodeInstruction 0x6E = InstructionHeader ROR Absolute
decodeInstruction 0x70 = InstructionHeader BVS Relative
decodeInstruction 0x71 = InstructionHeader ADC IndirectIndexed
decodeInstruction 0x75 = InstructionHeader ADC ZeroPageX
decodeInstruction 0x76 = InstructionHeader ROR ZeroPageX
decodeInstruction 0x78 = InstructionHeader SEI Implicit
decodeInstruction 0x79 = InstructionHeader ADC AbsoluteY
decodeInstruction 0x7D = InstructionHeader ADC AbsoluteX
decodeInstruction 0x7E = InstructionHeader ROR AbsoluteX
decodeInstruction 0x81 = InstructionHeader STA IndexedIndirect
decodeInstruction 0x84 = InstructionHeader STY ZeroPage
decodeInstruction 0x85 = InstructionHeader STA ZeroPage
decodeInstruction 0x86 = InstructionHeader STX ZeroPage
decodeInstruction 0x88 = InstructionHeader DEY Implicit
decodeInstruction 0x8A = InstructionHeader TXA Implicit
decodeInstruction 0x8C = InstructionHeader STY Absolute
decodeInstruction 0x8D = InstructionHeader STA Absolute
decodeInstruction 0x8E = InstructionHeader STX Absolute
decodeInstruction 0x90 = InstructionHeader BCC Relative
decodeInstruction 0x91 = InstructionHeader STA IndirectIndexed
decodeInstruction 0x94 = InstructionHeader STY ZeroPageX
decodeInstruction 0x95 = InstructionHeader STA ZeroPageX
decodeInstruction 0x96 = InstructionHeader STX ZeroPageX
decodeInstruction 0x98 = InstructionHeader TYA Implicit
decodeInstruction 0x99 = InstructionHeader STA AbsoluteY
decodeInstruction 0x9A = InstructionHeader TXS Implicit
decodeInstruction 0x9D = InstructionHeader STA AbsoluteX
decodeInstruction 0xA0 = InstructionHeader LDY Immediate
decodeInstruction 0xA1 = InstructionHeader LDA IndexedIndirect
decodeInstruction 0xA2 = InstructionHeader LDX Immediate
decodeInstruction 0xA4 = InstructionHeader LDY ZeroPage
decodeInstruction 0xA5 = InstructionHeader LDA ZeroPage
decodeInstruction 0xA6 = InstructionHeader LDX ZeroPage
decodeInstruction 0xA8 = InstructionHeader TAY Implicit
decodeInstruction 0xA9 = InstructionHeader LDA Immediate
decodeInstruction 0xAA = InstructionHeader TAX Implicit
decodeInstruction 0xAC = InstructionHeader LDY Absolute
decodeInstruction 0xAD = InstructionHeader LDA Absolute
decodeInstruction 0xAE = InstructionHeader LDX Absolute
decodeInstruction 0xB0 = InstructionHeader BCS Relative
decodeInstruction 0xB1 = InstructionHeader LDA IndirectIndexed
decodeInstruction 0xB4 = InstructionHeader LDY ZeroPageX
decodeInstruction 0xB5 = InstructionHeader LDA ZeroPageX
decodeInstruction 0xB6 = InstructionHeader LDX ZeroPageX
decodeInstruction 0xB8 = InstructionHeader CLV Implicit
decodeInstruction 0xB9 = InstructionHeader LDA AbsoluteY
decodeInstruction 0xBA = InstructionHeader TSX Implicit
decodeInstruction 0xBC = InstructionHeader LDY AbsoluteX
decodeInstruction 0xBD = InstructionHeader LDA AbsoluteX
decodeInstruction 0xBE = InstructionHeader LDX AbsoluteY
decodeInstruction 0xC0 = InstructionHeader CPY Immediate
decodeInstruction 0xC1 = InstructionHeader CMP IndexedIndirect
decodeInstruction 0xC4 = InstructionHeader CPY ZeroPage
decodeInstruction 0xC5 = InstructionHeader CMP ZeroPage
decodeInstruction 0xC6 = InstructionHeader DEC ZeroPage
decodeInstruction 0xC8 = InstructionHeader INY Implicit
decodeInstruction 0xC9 = InstructionHeader CMP Immediate
decodeInstruction 0xCA = InstructionHeader DEX Implicit
decodeInstruction 0xCC = InstructionHeader CPY Absolute
decodeInstruction 0xCD = InstructionHeader CMP Absolute
decodeInstruction 0xCE = InstructionHeader DEC Absolute
decodeInstruction 0xD0 = InstructionHeader BNE Relative
decodeInstruction 0xD1 = InstructionHeader CMP IndirectIndexed
decodeInstruction 0xD5 = InstructionHeader CMP ZeroPageX
decodeInstruction 0xD6 = InstructionHeader DEC ZeroPageX
decodeInstruction 0xD8 = InstructionHeader CLD Implicit
decodeInstruction 0xD9 = InstructionHeader CMP AbsoluteY
decodeInstruction 0xDD = InstructionHeader CMP AbsoluteX
decodeInstruction 0xDE = InstructionHeader DEC AbsoluteX
decodeInstruction 0xE0 = InstructionHeader CPX Immediate
decodeInstruction 0xE1 = InstructionHeader SBC IndexedIndirect
decodeInstruction 0xE4 = InstructionHeader CPX ZeroPage
decodeInstruction 0xE5 = InstructionHeader SBC ZeroPage
decodeInstruction 0xE6 = InstructionHeader INC ZeroPage
decodeInstruction 0xE8 = InstructionHeader INX Implicit
decodeInstruction 0xE9 = InstructionHeader SBC Immediate
decodeInstruction 0xEA = InstructionHeader NOP Implicit
decodeInstruction 0xEC = InstructionHeader CPX Absolute
decodeInstruction 0xED = InstructionHeader SBC Absolute
decodeInstruction 0xEE = InstructionHeader INC Absolute
decodeInstruction 0xF0 = InstructionHeader BEQ Relative
decodeInstruction 0xF1 = InstructionHeader SBC IndirectIndexed
decodeInstruction 0xF5 = InstructionHeader SBC ZeroPageX
decodeInstruction 0xF6 = InstructionHeader INC ZeroPageX
decodeInstruction 0xF8 = InstructionHeader SED Implicit
decodeInstruction 0xF9 = InstructionHeader SBC AbsoluteY
decodeInstruction 0xFD = InstructionHeader SBC AbsoluteX
decodeInstruction 0xFE = InstructionHeader INC AbsoluteX
decodeInstruction i = error $ "Illegal opcode: 0x" ++ showHex i ""

data Operand = DataOperand Word8 | AddressOperand Address | NoOperand
  deriving (Show)

data Instruction = Instruction Opcode AddressingMode Operand

instance Show Instruction where
  show :: Instruction -> String
  show (Instruction op Implicit NoOperand) = show op
  show (Instruction op Accumulator NoOperand) = show op ++ " A"
  show (Instruction op Immediate (DataOperand val)) = show op ++ " #$" ++ showHex val ""
  show (Instruction op ZeroPage (DataOperand val)) = show op ++ " $" ++ showHex val ""
  show (Instruction op ZeroPageX (DataOperand val)) = show op ++ " $" ++ showHex val ",X"
  show (Instruction op ZeroPageY (DataOperand val)) = show op ++ " $" ++ showHex val ",Y"
  show (Instruction op Relative (DataOperand val)) = show op ++ " $" ++ showHex val ""
  show (Instruction op Absolute (AddressOperand addr)) = show op ++ " $" ++ showHex addr ""
  show (Instruction op AbsoluteX (AddressOperand addr)) = show op ++ " $" ++ showHex addr ",X"
  show (Instruction op AbsoluteY (AddressOperand addr)) = show op ++ " $" ++ showHex addr ",Y"
  show (Instruction op Indirect (AddressOperand addr)) = show op ++ " ($" ++ showHex addr ")"
  show (Instruction op IndexedIndirect (DataOperand val)) = show op ++ " ($" ++ showHex val ",X)"
  show (Instruction op IndirectIndexed (DataOperand val)) = show op ++ " ($" ++ showHex val "),Y"