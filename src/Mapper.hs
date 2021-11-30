{-# LANGUAGE MultiWayIf #-}

module Mapper (Mapper(..), runMapper) where

import MemoryBank (BankAddress (..), BankType (..))

import Data.Word  (Word16)

data Mapper = Mapper0 deriving (Show)

type MapperFunc = Word16 -> BankAddress

mapper0 :: MapperFunc
mapper0 addr =
    if | 0x6000 <= addr && addr <= 0x7FFF -> BankAddress PrgRam (addr - 0x6000)
       | 0x8000 <= addr && addr <= 0xBFFF -> BankAddress MainRom (addr - 0x8000)
       | 0xC000 <= addr && addr <= 0xFFFF -> BankAddress MainRom (addr - 0xC000)
       | otherwise -> error $ "No mapping for address " ++ show addr

runMapper :: Mapper -> MapperFunc
runMapper Mapper0 = mapper0
