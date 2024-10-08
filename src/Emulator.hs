{-# LANGUAGE ImpredicativeTypes #-}

module Emulator (Emulator, newEmulator, stepEmulator) where

import Bits (fromBool, packWord16, toWord16)
import CPU (CPU, StatusFlag, carryFlag, decimalFlag, interruptFlag, negativeFlag, newCpu, overflowFlag, zeroFlag)
import Control.Arrow ((>>>))
import qualified Control.Lens as L
import Control.Lens.Operators ((%=), (.=), (<%=), (<&>), (<+=), (<-=), (<<+=), (<<-=), (<<.=), (^.))
import Control.Monad (liftM2, unless, when)
import Control.Monad.State (MonadState, gets)
import Data.Bits (shiftL, shiftR, testBit, xor, (.&.))
import Data.Bits.Lens ((<.&.=), (<.|.=))
import qualified Data.Bits.Lens as B
import Data.Word (Word16, Word8)
import GHC.Generics (Generic)
import GHC.Num (subtract)
import Instructions (AddressingMode (..), Instruction (..), InstructionHeader (..), Opcode (..), Operand (..), decodeInstruction)
import Memory (Memory, memoryIx, newMemory, readWord16, stackPage)
import Types (Address)

data Emulator = Emulator
  { cpu :: CPU,
    memory :: Memory
  }
  deriving (Generic, Show)

newEmulator :: Emulator
newEmulator =
  Emulator
    { cpu = newCpu,
      memory = newMemory
    }

readPC :: (MonadState Emulator m) => m Word8
readPC = do
  oldPC <- #cpu . #pc <<+= 1
  L.use $ #memory . memoryIx oldPC

readPCWord16 :: (MonadState Emulator m) => m Word16
readPCWord16 = liftM2 (,) readPC readPC <&> L.view packWord16

stepEmulator :: (MonadState Emulator m) => m Instruction
stepEmulator = do
  (InstructionHeader opcode mode) <- decodeInstruction <$> readPC
  operand <- readOperand mode
  target <- gets $ toTarget mode operand
  executeInstruction opcode target
  return $ Instruction opcode mode operand

readOperand :: (MonadState Emulator m) => AddressingMode -> m Operand
readOperand Implicit = return NoOperand
readOperand Accumulator = return NoOperand
readOperand Immediate = DataOperand <$> readPC
readOperand ZeroPage = DataOperand <$> readPC
readOperand ZeroPageX = DataOperand <$> readPC
readOperand ZeroPageY = DataOperand <$> readPC
readOperand Relative = DataOperand <$> readPC
readOperand Absolute = AddressOperand <$> readPCWord16
readOperand AbsoluteX = AddressOperand <$> readPCWord16
readOperand AbsoluteY = AddressOperand <$> readPCWord16
readOperand Indirect = AddressOperand <$> readPCWord16
readOperand IndexedIndirect = DataOperand <$> readPC
readOperand IndirectIndexed = DataOperand <$> readPC

type InstructionTarget = (Maybe Address, L.ReifiedLens' Emulator Word8)

toTarget :: AddressingMode -> Operand -> Emulator -> InstructionTarget
toTarget Implicit NoOperand _ = error "Trying to access the operand of an instruction with no operand"
toTarget Accumulator NoOperand _ = (Nothing, L.Lens $ #cpu . #a)
toTarget Immediate (DataOperand val) _ =
  (Nothing, L.Lens $ L.lens (const val) const)
toTarget ZeroPage (DataOperand val) _ = do
  let addr = toWord16 val
  (Just addr, L.Lens $ #memory . memoryIx addr)
toTarget ZeroPageX (DataOperand val) e = do
  let x = e ^. #cpu . #x
  let addr = toWord16 $ val + x
  (Just addr, L.Lens $ #memory . memoryIx addr)
toTarget ZeroPageY (DataOperand val) e = do
  let y = e ^. #cpu . #y
  let addr = toWord16 $ val + y
  (Just addr, L.Lens $ #memory . memoryIx addr)
toTarget Relative (DataOperand offset) e = do
  let pc = e ^. #cpu . #pc
  let signedDiff = if offset < 0x80 then 0 else 0x100 -- TODO: signed arithmetic function?
  let addr = pc + toWord16 offset - signedDiff
  (Just addr, L.Lens $ #memory . memoryIx addr)
toTarget Absolute (AddressOperand addr) _ =
  (Just addr, L.Lens $ #memory . memoryIx addr)
toTarget AbsoluteX (AddressOperand base) e = do
  let x = e ^. #cpu . #x
  let addr = base + toWord16 x
  (Just addr, L.Lens $ #memory . memoryIx addr)
toTarget AbsoluteY (AddressOperand base) e = do
  let y = e ^. #cpu . #y
  let addr = base + toWord16 y
  (Just addr, L.Lens $ #memory . memoryIx addr)
toTarget Indirect (AddressOperand firstAddr) e = do
  let secondAddr = e ^. #memory . readWord16 firstAddr
  (Just secondAddr, L.Lens $ #memory . memoryIx secondAddr)
toTarget IndexedIndirect (DataOperand firstAddr) e = do
  let x = e ^. #cpu . #x
  let secondAddr = e ^. #memory . readWord16 (toWord16 $ firstAddr + x)
  (Just secondAddr, L.Lens $ #memory . memoryIx secondAddr)
toTarget IndirectIndexed (DataOperand firstAddr) e = do
  let y = e ^. #cpu . #y
  let secondBase = e ^. #memory . readWord16 (toWord16 firstAddr)
  let secondAddr = secondBase + toWord16 y
  (Just secondAddr, L.Lens $ #memory . memoryIx secondAddr)

executeInstruction :: (MonadState Emulator m) => Opcode -> InstructionTarget -> m ()
executeInstruction ADC (_, L.Lens op) = do
  val <- L.use op
  a <- L.use $ #cpu . #a
  carry <- L.uses (#cpu . carryFlag) fromBool
  result <- #cpu . #a <+= val + carry
  setCarryFlag $ toWord16 val + toWord16 a + toWord16 carry
  setOverflowFlag val a result
  setZNFlags result
executeInstruction AND (_, L.Lens op) = do
  val <- L.use op
  result <- #cpu . #a <.&.= val
  setZNFlags result
executeInstruction ASL (_, L.Lens op) = do
  newCarry <- L.use $ op . B.bitAt 7
  #cpu . carryFlag .= newCarry
  result <- op <%= (`shiftL` 1)
  setZNFlags result
executeInstruction BCC (Just addr, _) = branchIfFlagClear carryFlag addr
executeInstruction BCS (Just addr, _) = branchIfFlagSet carryFlag addr
executeInstruction BEQ (Just addr, _) = branchIfFlagSet zeroFlag addr
executeInstruction BIT (_, L.Lens op) = do
  val <- L.use op
  a <- L.use $ #cpu . #a
  #cpu . zeroFlag .= ((val .&. a) == 0)
  #cpu . overflowFlag .= (val `testBit` 6)
  #cpu . negativeFlag .= (val `testBit` 7)
executeInstruction BMI (Just addr, _) = branchIfFlagSet negativeFlag addr
executeInstruction BNE (Just addr, _) = branchIfFlagClear zeroFlag addr
executeInstruction BPL (Just addr, _) = branchIfFlagClear negativeFlag addr
executeInstruction BRK _ = return () -- TODO: implement BRK
executeInstruction BVC (Just addr, _) = branchIfFlagClear overflowFlag addr
executeInstruction BVS (Just addr, _) = branchIfFlagSet overflowFlag addr
executeInstruction CLC _ = #cpu . carryFlag .= False
executeInstruction CLD _ = #cpu . decimalFlag .= False
executeInstruction CLI _ = #cpu . interruptFlag .= False
executeInstruction CLV _ = #cpu . overflowFlag .= False
executeInstruction CMP (_, L.Lens op) = do
  val <- L.use op
  a <- L.use $ #cpu . #a
  setCmpFlags a val
executeInstruction CPX (_, L.Lens op) = do
  val <- L.use op
  x <- L.use $ #cpu . #x
  setCmpFlags x val
executeInstruction CPY (_, L.Lens op) = do
  val <- L.use op
  y <- L.use $ #cpu . #y
  setCmpFlags y val
executeInstruction DEC (_, L.Lens op) = do
  result <- op <-= 1
  setZNFlags result
executeInstruction DEX _ = do
  result <- #cpu . #x <-= 1
  setZNFlags result
executeInstruction DEY _ = do
  result <- #cpu . #y <-= 1
  setZNFlags result
executeInstruction EOR (_, L.Lens op) = do
  val <- L.use op
  result <- #cpu . #a <%= (`xor` val)
  setZNFlags result
executeInstruction INC (_, L.Lens op) = do
  result <- op <+= 1
  setZNFlags result
executeInstruction INX _ = do
  result <- #cpu . #x <+= 1
  setZNFlags result
executeInstruction INY _ = do
  result <- #cpu . #y <+= 1
  setZNFlags result
executeInstruction JMP (Just addr, _) = #cpu . #pc .= addr
executeInstruction JSR (Just addr, _) = do
  (pcLo, pcHi) <- L.use $ #cpu . #pc . L.to (subtract 1) . L.from packWord16
  pushStack pcHi
  pushStack pcLo
  #cpu . #pc .= addr
executeInstruction LDA (_, L.Lens op) = do
  val <- L.use op
  #cpu . #a .= val
  setZNFlags val
executeInstruction LDX (_, L.Lens op) = do
  val <- L.use op
  #cpu . #x .= val
  setZNFlags val
executeInstruction LDY (_, L.Lens op) = do
  val <- L.use op
  #cpu . #y .= val
  setZNFlags val
executeInstruction LSR (_, L.Lens op) = do
  newCarry <- L.use $ op . B.bitAt 0
  #cpu . carryFlag .= newCarry
  result <- op <%= ((`shiftR` 1) >>> (.&. 0x7F))
  setZNFlags result
executeInstruction NOP _ = return ()
executeInstruction ORA (_, L.Lens op) = do
  val <- L.use op
  result <- #cpu . #a <.|.= val
  setZNFlags result
executeInstruction PHA _ = L.use (#cpu . #a) >>= pushStack
executeInstruction PHP _ = L.use (#cpu . #status . #toWord) >>= pushStack
executeInstruction PLA _ = do
  val <- popStack
  #cpu . #a .= val
  setZNFlags val
executeInstruction PLP _ = popStack >>= L.assign (#cpu . #status . #toWord)
executeInstruction ROL (_, L.Lens op) = do
  newCarry <- L.use $ op . B.bitAt 7
  oldCarry <- #cpu . carryFlag <<.= newCarry
  result <- op <%= (`shiftL` 1)
  op . B.bitAt 0 .= oldCarry
  setZNFlags result
executeInstruction ROR (_, L.Lens op) = do
  newCarry <- L.use $ op . B.bitAt 0
  oldCarry <- #cpu . carryFlag <<.= newCarry
  result <- op <%= (`shiftR` 1)
  op . B.bitAt 7 .= oldCarry
  setZNFlags result
executeInstruction RTI _ = return () -- TODO: implement RTI
executeInstruction RTS _ = do
  pcLo <- popStack
  pcHi <- popStack
  #cpu . #pc .= L.views packWord16 (+ 1) (pcLo, pcHi)
executeInstruction SBC (_, L.Lens op) = do
  val <- L.use op
  a <- L.use $ #cpu . #a
  carry <- L.uses (#cpu . carryFlag) fromBool
  result <- #cpu . #a <-= val + (1 - carry)
  setCarryFlag $ toWord16 a - toWord16 val - toWord16 (1 - carry)
  #cpu . carryFlag %= not -- TODO: fix this
  setOverflowFlag val a result
  setZNFlags result
executeInstruction SEC _ = #cpu . carryFlag .= True
executeInstruction SED _ = #cpu . decimalFlag .= True
executeInstruction SEI _ = #cpu . interruptFlag .= True
executeInstruction STA (_, L.Lens op) = L.use (#cpu . #a) >>= L.assign op
executeInstruction STX (_, L.Lens op) = L.use (#cpu . #x) >>= L.assign op
executeInstruction STY (_, L.Lens op) = L.use (#cpu . #y) >>= L.assign op
executeInstruction TAX _ = do
  a <- L.use $ #cpu . #a
  #cpu . #x .= a
  setZNFlags a
executeInstruction TAY _ = do
  a <- L.use $ #cpu . #a
  #cpu . #y .= a
  setZNFlags a
executeInstruction TSX _ = do
  sp <- L.use $ #cpu . #sp
  #cpu . #x .= sp
  setZNFlags sp
executeInstruction TXA _ = do
  x <- L.use $ #cpu . #x
  #cpu . #a .= x
  setZNFlags x
executeInstruction TXS _ = L.use (#cpu . #x) >>= L.assign (#cpu . #sp)
executeInstruction TYA _ = do
  y <- L.use $ #cpu . #y
  #cpu . #a .= y
  setZNFlags y

-- TODO: factor out more coomon functions?
branchIfFlagSet :: (MonadState Emulator m) => StatusFlag -> Address -> m ()
branchIfFlagSet flag addr = do
  flagVal <- L.use $ #cpu . flag
  when flagVal $ #cpu . #pc .= addr

branchIfFlagClear :: (MonadState Emulator m) => StatusFlag -> Address -> m ()
branchIfFlagClear flag addr = do
  flagVal <- L.use $ #cpu . flag
  unless flagVal $ #cpu . #pc .= addr

pushStack :: (MonadState Emulator m) => Word8 -> m ()
pushStack val = do
  oldSP <- #cpu . #sp <<-= 1
  let stackAddr = L.view packWord16 (oldSP, stackPage)
  #memory . memoryIx stackAddr .= val

popStack :: (MonadState Emulator m) => m Word8
popStack = do
  newSP <- #cpu . #sp <+= 1
  let stackAddr = L.view packWord16 (newSP, stackPage)
  L.use $ #memory . memoryIx stackAddr

-- TODO: move these out of the state monad?
setZNFlags :: (MonadState Emulator m) => Word8 -> m ()
setZNFlags val = do
  #cpu . zeroFlag .= (val == 0)
  #cpu . negativeFlag .= val ^. B.bitAt 7

setCmpFlags :: (MonadState Emulator m) => Word8 -> Word8 -> m ()
setCmpFlags reg operand = do
  #cpu . carryFlag .= (reg >= operand)
  setZNFlags $ reg - operand

setOverflowFlag :: (MonadState Emulator m) => Word8 -> Word8 -> Word8 -> m ()
setOverflowFlag op1 op2 result = do
  let opsSameSign = not $ (op1 `xor` op2) `testBit` 7
  let resultOppositeSign = (op1 `xor` result) `testBit` 7
  #cpu . overflowFlag .= (opsSameSign && resultOppositeSign)

setCarryFlag :: (MonadState Emulator m) => Word16 -> m ()
setCarryFlag result = #cpu . carryFlag .= (result .&. 0xFF00 /= 0)