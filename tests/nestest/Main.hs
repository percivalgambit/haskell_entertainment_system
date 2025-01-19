{-# LANGUAGE OverloadedLabels #-}

module Main where

import CPU (CPU (..), showRegisters)
import Control.Arrow ((>>>))
import qualified Control.Lens as L
import Control.Lens.Operators ((.~), (^.))
import Control.Monad (unless)
import Control.Monad.State (runStateT)
import Data.Char (toUpper)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (intercalate)
import Data.Word (Word8)
import Emulator (Emulator (..), newEmulator, stepEmulator)
import Instructions (Instruction (..), Opcode (..), instructionBytes)
import Rom (newRom)
import System.Log.Logger (Logger, Priority (INFO), getLogger, logL)
import qualified System.Log.Logger as Log
import Text.Printf (printf)

loggerName :: String
loggerName = "nestest"

main :: IO ()
main = do
  logger <- getLogger loggerName <&> Log.setLevel INFO
  rom <- newRom "tests/nestest/nestest.nes"
  let emulator = newEmulator rom & #cpu . #pc .~ 0xC000
  testLoop logger emulator
  Log.removeAllHandlers

testLoop :: Logger -> Emulator -> IO ()
testLoop logger emulator = do
  let programCounter = emulator ^. #cpu . #pc . L.to (printf "%04X")
  (instruction, emulator') <- runStateT stepEmulator emulator
  let bytes = instructionBytes instruction <&> printf "%02X" & unwords & padWithSpace 8
  -- TODO: Add mem references to logging output
  let instructionStr = show instruction & padWithSpace 30
  let cpuRegisters = emulator ^. #cpu . L.to showRegisters
  logL logger INFO $ intercalate "  " [programCounter, bytes, instructionStr, cpuRegisters]
  unless (isBrk instruction) $ testLoop logger emulator'

padWithSpace :: Int -> String -> String
padWithSpace = fitToLengthBack ' '

fitToLengthFront :: Char -> Int -> String -> String
fitToLengthFront fillChar staticLength str = lengthCorrectedStr
  where
    lengthDiff = staticLength - length str
    lengthCorrectedStr
      | lengthDiff > 0 = replicate lengthDiff fillChar ++ str
      | lengthDiff < 0 = drop lengthDiff str
      | otherwise = str

fitToLengthBack :: Char -> Int -> String -> String
fitToLengthBack fillChar staticLength str = lengthCorrectedStr
  where
    lengthDiff = staticLength - length str
    lengthCorrectedStr
      | lengthDiff > 0 = str ++ replicate lengthDiff fillChar
      | lengthDiff < 0 = take staticLength str
      | otherwise = str

isBrk :: Instruction -> Bool
isBrk (Instruction {opcode = BRK}) = True
isBrk _ = False