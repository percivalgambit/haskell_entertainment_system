{-# LANGUAGE Rank2Types #-}

module CpuExecution (stepCpu) where

import Cpu                 (HasCpu (..), HasFlags (..))
import Instruction         (Instruction (..), Opcode (..), Operand (..),
                            decodeInstruction)
import Memory              (Address, HasMemory (..))
import Util                (getHighWord8, getLowWord8)


import Control.Category    ((>>>))
import Control.Lens        (Getter, Lens', Setter', assign, use, uses, (&),
                            (.=), (<%=), (<+=), (<-=), (<<+=), (<<-=))
import Control.Monad       (unless, when)
import Control.Monad.State (State)
import Data.Bits           (Bits, rotateR, shiftL, xor, (.&.), (.|.))
import Data.Bits.Lens      ((<.&.=), (<.|.=))
import Data.Int            (Int16, Int8)
import Data.Word           (Word16, Word8)

stepCpu :: HasCpu c => State c ()
stepCpu = decodeInstruction >>= executeInstruction

executeInstruction :: HasCpu c => Instruction -> State c ()
executeInstruction (Instruction ADC (AddressOperand address)) =
    executeWithDataAtAddress ADC address
executeInstruction (Instruction ADC (DataOperand w8)) = do
    acc <- uses accumulator fromIntegral :: HasCpu c => State c Int8
    carryBit <- uses carry fromEnum
    let i8 = fromIntegral w8 :: Int8
    let newAcc = fromIntegral acc + fromIntegral carryBit + fromIntegral i8 :: Int16
    accumulator .= fromIntegral newAcc
    carry .= hasCarryAdd (fromIntegral newAcc)
    zero .= isZero newAcc
    overflow .= hasOverflow acc i8 (fromIntegral newAcc)
    negative .= isNegative newAcc
executeInstruction (Instruction AND (AddressOperand address)) =
    executeWithDataAtAddress AND address
executeInstruction (Instruction AND (DataOperand w8)) = do
    newAcc <- accumulator <.&.= w8
    zero .= isZero newAcc
    negative .= isNegative newAcc
executeInstruction (Instruction ASL (AddressOperand address)) =
    shiftLeft $ memoryByte address
executeInstruction (Instruction ASL (DataOperand _)) =
    shiftLeft accumulator
executeInstruction (Instruction BCC (AddressOperand address)) =
    branchIfFlagClear carry address
executeInstruction (Instruction BCS (AddressOperand address)) =
    branchIfFlagSet carry address
executeInstruction (Instruction BEQ (AddressOperand address)) =
    branchIfFlagSet zero address
executeInstruction (Instruction BIT (AddressOperand address)) = do
    acc <- use accumulator
    w8 <- use $ memoryByte address
    let result = acc .&. w8
    zero .= isZero result
    overflow .= hasOverflowBit result
    negative .= isNegative result
executeInstruction (Instruction BMI (AddressOperand address)) =
    branchIfFlagSet negative address
executeInstruction (Instruction BNE (AddressOperand address)) =
    branchIfFlagClear zero address
executeInstruction (Instruction BPL (AddressOperand address)) =
    branchIfFlagClear negative address
-- TODO: Implement BRK
executeInstruction (Instruction BRK NoOperand) =
    breakCommand .= True
executeInstruction (Instruction BVC (AddressOperand address)) =
    branchIfFlagClear overflow address
executeInstruction (Instruction BVS (AddressOperand address)) =
    branchIfFlagSet overflow address
executeInstruction (Instruction CLC NoOperand) =
    carry .= False
executeInstruction (Instruction CLD NoOperand) =
    decimal .= False
executeInstruction (Instruction CLI NoOperand) =
    interruptDisable .= False
executeInstruction (Instruction CLV NoOperand) =
    overflow .= False
executeInstruction (Instruction CMP (AddressOperand address)) =
    executeWithDataAtAddress CMP address
executeInstruction (Instruction CMP (DataOperand w8)) =
    compareRegister accumulator w8
executeInstruction (Instruction CPX (AddressOperand address)) =
    executeWithDataAtAddress CPX address
executeInstruction (Instruction CPX (DataOperand w8)) =
    compareRegister indexX w8
executeInstruction (Instruction CPY (AddressOperand address)) =
    executeWithDataAtAddress CPY address
executeInstruction (Instruction CPY (DataOperand w8)) =
    compareRegister indexY w8
executeInstruction (Instruction DEC (AddressOperand address)) =
    decrement $ memoryByte address
executeInstruction (Instruction DEX NoOperand) =
    decrement indexX
executeInstruction (Instruction DEY NoOperand) =
    decrement indexY
executeInstruction (Instruction EOR (AddressOperand address)) =
    executeWithDataAtAddress EOR address
executeInstruction (Instruction EOR (DataOperand w8)) = do
    newAcc <- accumulator <%= xor w8
    zero .= isZero newAcc
    negative .= isNegative newAcc
executeInstruction (Instruction INC (AddressOperand address)) =
    increment $ memoryByte address
executeInstruction (Instruction INX NoOperand) =
    increment indexX
executeInstruction (Instruction INY NoOperand) =
    increment indexY
executeInstruction (Instruction JMP (AddressOperand address)) =
    programCounter .= address
executeInstruction (Instruction JSR (AddressOperand address)) = do
    pc <- uses programCounter (subtract 1)
    stackPush $ getHighWord8 pc
    stackPush $ getLowWord8 pc
    programCounter .= address
executeInstruction (Instruction LDA (AddressOperand address)) =
    executeWithDataAtAddress LDA address
executeInstruction (Instruction LDA (DataOperand w8)) =
    loadData accumulator w8
executeInstruction (Instruction LDX (AddressOperand address)) =
    executeWithDataAtAddress LDX address
executeInstruction (Instruction LDX (DataOperand w8)) =
    loadData indexX w8
executeInstruction (Instruction LDY (AddressOperand address)) =
    executeWithDataAtAddress LDY address
executeInstruction (Instruction LDY (DataOperand w8)) =
    loadData indexY w8
executeInstruction (Instruction LSR (AddressOperand address)) =
    shiftRight $ memoryByte address
executeInstruction (Instruction LSR (DataOperand _)) =
    shiftRight accumulator
executeInstruction (Instruction NOP NoOperand) = return ()
executeInstruction (Instruction ORA (AddressOperand address)) =
    executeWithDataAtAddress ORA address
executeInstruction (Instruction ORA (DataOperand w8)) = do
    newAcc <- accumulator <.|.= w8
    zero .= isZero newAcc
    negative .= isNegative newAcc
executeInstruction (Instruction PHA NoOperand) =
    use accumulator >>= stackPush
executeInstruction (Instruction PHP NoOperand) =
    use flagByte >>= stackPush
executeInstruction (Instruction PLA NoOperand) = do
    acc <- stackPop
    accumulator .= acc
    zero .= isZero acc
    negative .= isNegative acc
executeInstruction (Instruction PLP NoOperand) =
    stackPop >>= assign flagByte
executeInstruction (Instruction ROL (AddressOperand address)) =
    rotateLeft $ memoryByte address
executeInstruction (Instruction ROL (DataOperand _)) =
    rotateLeft accumulator
executeInstruction (Instruction ROR (AddressOperand address)) =
    rotateRight $ memoryByte address
executeInstruction (Instruction ROR (DataOperand _)) =
    rotateRight accumulator
-- TODO: Implement RTI
executeInstruction (Instruction RTI NoOperand) = do
    stackPop >>= assign flagByte
    stackPop >>= (fromIntegral >>> assign programCounter)
executeInstruction (Instruction RTS NoOperand) = do
    lowByte <- stackPop
    highByte <- stackPop
    let pc = (fromIntegral highByte :: Word16) `shiftL` 8 .|. (fromIntegral lowByte :: Word16) + 1
    programCounter .= pc
executeInstruction (Instruction SBC (AddressOperand address)) =
    executeWithDataAtAddress SBC address
executeInstruction (Instruction SBC (DataOperand w8)) = do
    acc <- uses accumulator fromIntegral :: HasCpu c => State c Int8
    carryBit <- uses carry $ not >>> fromEnum
    let i8 = w8 & fromIntegral :: Int8
    let newAcc = fromIntegral acc - fromIntegral i8 - fromIntegral carryBit :: Int16
    accumulator .= fromIntegral newAcc
    carry .= hasCarrySub (fromIntegral newAcc)
    zero .= isZero newAcc
    overflow .= hasOverflow acc i8 (fromIntegral newAcc)
    negative .= isNegative newAcc
executeInstruction (Instruction SEC NoOperand) =
    carry .= True
executeInstruction (Instruction SED NoOperand) =
    decimal .= True
executeInstruction (Instruction SEI NoOperand) =
    interruptDisable .= True
executeInstruction (Instruction STA (AddressOperand address)) =
    storeRegister accumulator $ memoryByte address
executeInstruction (Instruction STX (AddressOperand address)) =
    storeRegister indexX $ memoryByte address
executeInstruction (Instruction STY (AddressOperand address)) =
    storeRegister indexY $ memoryByte address
executeInstruction (Instruction TAX NoOperand) =
    transferRegister accumulator indexX
executeInstruction (Instruction TAY NoOperand) =
    transferRegister accumulator indexY
executeInstruction (Instruction TSX NoOperand) =
    transferRegister stackPointer indexX
executeInstruction (Instruction TXA NoOperand) =
    transferRegister indexX accumulator
executeInstruction (Instruction TXS NoOperand) =
    transferRegister indexX stackPointer
executeInstruction (Instruction TYA NoOperand) =
    transferRegister indexY accumulator
-- TODO: Propagate errors better and add a better error message
executeInstruction _ = fail "Unknown opcode found"

shiftLeft :: HasCpu c => Lens' c Word8 -> State c ()
shiftLeft l = do
    value <- use l
    let newValue = (fromIntegral value :: Word16) `shiftL` 1
    l .= fromIntegral newValue
    carry .= hasCarryAdd newValue
    zero .= isZero newValue
    negative .= isNegative newValue

shiftRight :: HasCpu c => Lens' c Word8 -> State c ()
shiftRight l = do
    value <- use l
    let newValue = (fromIntegral value :: Word16) `rotateR` 1
    l .= fromIntegral newValue
    carry .= hasCarryAdd newValue
    zero .= isZero newValue
    negative .= isNegative newValue

rotateLeft :: HasCpu c => Lens' c Word8 -> State c ()
rotateLeft l = do
    value <- use l
    carryFlag <-  uses carry $ fromEnum >>> fromIntegral
    let newValue = ((fromIntegral value :: Word16) `shiftL` 1) .|. carryFlag
    l .= fromIntegral newValue
    carry .= hasCarryAdd newValue
    zero .= isZero newValue
    negative .= isNegative newValue

rotateRight :: HasCpu c => Lens' c Word8 -> State c ()
rotateRight l = do
    value <- use l
    carryFlag <-  uses carry $ fromEnum >>> fromIntegral
    let newValue = ((fromIntegral value :: Word16) `rotateR` 1) .|. (carryFlag `shiftL` 7)
    l .= fromIntegral newValue
    carry .= hasCarryAdd newValue
    zero .= isZero newValue
    negative .= isNegative newValue

branchIfFlagSet :: HasCpu c => Getter c Bool -> Address -> State c ()
branchIfFlagSet l address = do
    flag <- use l
    when flag $ programCounter .= address

branchIfFlagClear :: HasCpu c => Getter c Bool -> Address -> State c ()
branchIfFlagClear l address = do
    flag <- use l
    unless flag $ programCounter .= address

compareRegister :: HasCpu c => Getter c Word8 -> Word8 -> State c ()
compareRegister l w8 = do
    reg <- use l
    let result = fromIntegral reg - fromIntegral w8
    carry .= hasCarrySub result
    zero .= isZero result
    negative .= isNegative result

decrement :: HasCpu c => Lens' c Word8 -> State c ()
decrement l = do
    newReg <- l <-= 1
    zero .= isZero newReg
    negative .= isNegative newReg

increment :: HasCpu c => Lens' c Word8 -> State c ()
increment l = do
    newReg <- l <+= 1
    zero .= isZero newReg
    negative .= isNegative newReg

loadData :: HasCpu c => Setter' c Word8 -> Word8 -> State c ()
loadData l w8 = do
    l .= w8
    zero .= isZero w8
    negative .= isNegative w8

storeRegister :: HasCpu c => Lens' c Word8 -> Lens' c Word8 -> State c ()
storeRegister g s = use g >>= assign s

transferRegister :: HasCpu c => Lens' c Word8 -> Lens' c Word8 -> State c ()
transferRegister g s = do
    reg <- use g
    s .= reg
    zero .= isZero reg
    negative .= isNegative reg

executeWithDataAtAddress :: HasCpu c => Opcode -> Address -> State c ()
executeWithDataAtAddress opcode address = do
    w8 <- use $ memoryByte address
    executeInstruction $ Instruction opcode $ DataOperand w8

hasCarryAdd :: Word16 -> Bool
hasCarryAdd = (> fromIntegral (maxBound :: Word8))

hasCarrySub :: Word16 -> Bool
hasCarrySub = not . hasCarryAdd

isZero :: Integral a => a -> Bool
isZero i = (fromIntegral i :: Word8) == 0

isNegative :: Integral a => a -> Bool
isNegative i = (fromIntegral i :: Int8) < 0

hasOverflow :: Integral a => a -> a -> a -> Bool
hasOverflow op1 op2 result = sameSignOperands && differentSignResult
  where
    signedOp1 = fromIntegral op1 :: Int8
    signedOp2 = fromIntegral op2 :: Int8
    signedResult = fromIntegral result :: Int8
    sameSignOperands = (signedOp1 >= 0) == (signedOp2 >= 0)
    differentSignResult = (signedOp1 >= 0) /= (signedResult >= 0)

hasOverflowBit :: (Bits a, Num a) => a -> Bool
hasOverflowBit result = (result .&. 0x20) /= 0

stackPush :: HasCpu c => Word8 -> State c ()
stackPush w8 = do
    oldSp <- stackPointer <<+= 1
    memoryByte (getStackAddress oldSp) .= w8

stackPop :: HasCpu c => State c Word8
stackPop = do
    oldSp <- stackPointer <<-= 1
    use $ memoryByte (getStackAddress oldSp)

getStackAddress :: Word8 -> Word16
getStackAddress = (+ 0x100) . fromIntegral
