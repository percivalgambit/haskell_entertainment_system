{-# LANGUAGE Rank2Types #-}

module Cpu (HasCpu(..), HasFlags(..)) where

import Memory       (HasMemory)

import Control.Lens (Lens')
import Data.Word    (Word16, Word8)

class HasFlags f where
    flagByte :: Lens' f Word8
    carry :: Lens' f Bool
    zero :: Lens' f Bool
    interruptDisable :: Lens' f Bool
    decimal :: Lens' f Bool
    breakCommand :: Lens' f Bool
    overflow :: Lens' f Bool
    negative :: Lens' f Bool

class (HasMemory c, HasFlags c) => HasCpu c where
    programCounter :: Lens' c Word16
    stackPointer :: Lens' c Word8
    accumulator :: Lens' c Word8
    indexX :: Lens' c Word8
    indexY :: Lens' c Word8

