{-# LANGUAGE TemplateHaskell #-}

module MemoryBank (PrgRomData(..), ChrRomData(..), BankType(..), BankAddress(..), unPrgRomData, unChrRomData) where

import           Control.Lens    (makeLenses)
import qualified Data.ByteString as B
import           Data.Word       (Word16)

newtype PrgRomData = PrgRomData { _unPrgRomData :: B.ByteString } deriving (Show)
makeLenses ''PrgRomData
newtype ChrRomData = ChrRomData { _unChrRomData :: B.ByteString } deriving (Show)
makeLenses ''ChrRomData

data BankType = MainRom | MainRam | PrgRom | PrgRam | ChrRom | ChrRam deriving (Show)
data BankAddress = BankAddress BankType Word16 deriving (Show)
