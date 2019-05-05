{-# LANGUAGE Rank2Types #-}

module Memory (Address, HasMemory(..)) where

import Control.Lens (Lens')
import Data.Word    (Word16, Word8)

type Address = Word16

class HasMemory m where
    memoryByte :: Address -> Lens' m Word8
