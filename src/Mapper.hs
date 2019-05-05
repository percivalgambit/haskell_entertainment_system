{-# LANGUAGE MultiWayIf #-}

module Mapper (Mapper(..), runMapper, MappedAddress(..), BankType(..)) where

import Data.Word (Word16)

data Mapper = Mapper0 deriving (Show)

data BankType = MainRom | MainRam | PrgRom | PrgRam | ChrRom | ChrRam deriving (Show)
data MappedAddress = MappedAddress BankType Word16 deriving (Show)

type MapperFunc = Word16 -> MappedAddress

mapper0 :: MapperFunc
mapper0 addr =
    if | 0x6000 <= addr && addr <= 0x7FFF -> MappedAddress PrgRam (addr - 0x6000)
       | 0x8000 <= addr && addr <= 0xBFFF -> MappedAddress MainRom (addr - 0x8000)
       | 0xC000 <= addr && addr <= 0xFFFF -> MappedAddress MainRom (addr - 0xC000)
       | otherwise -> error $ "No mapping for address " ++ show addr

runMapper :: Mapper -> MapperFunc
runMapper Mapper0 = mapper0
