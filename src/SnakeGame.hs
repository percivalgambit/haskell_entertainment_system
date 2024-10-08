module SnakeGame (snakeGame, gameLoop) where

import Bits (packWord16, toWord16)
import Control.Arrow ((>>>))
import Control.Concurrent (threadDelay)
import qualified Control.Lens as L
import Control.Lens.Operators ((%=), (&), (.=), (.~), (<&>), (^.))
import Control.Monad (unless, when)
import Control.Monad.RWS (runRWST)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Lens as BS
import Data.Foldable (foldMap', foldl')
import Data.Function.Flip (flip3)
import Data.Maybe (fromMaybe)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Word (Word16, Word8)
import Emulator (Emulator (..), newEmulator, stepEmulator)
import Instructions (Instruction (..), Opcode (..))
import Memory (gameCodeAddress, memoryIx, memoryRange, readWord16)
import SDL (V2 (..), ($=))
import qualified SDL
import qualified SDL.Raw.Types as SDL (Color (..))
import System.Console.GetOpt (ArgDescr (..), ArgOrder (..), OptDescr (..), getOpt, usageInfo)
import System.Environment (getArgs, getProgName)
import qualified System.Log.Handler.Simple as Log
import System.Log.Logger (Logger, Priority (..), getLogger, logL, rootLoggerName, updateGlobalLogger)
import qualified System.Log.Logger as Log
import System.Random (StdGen, initStdGen, uniformR)
import Types (Address)

gameCode :: BS.ByteString
gameCode =
  [ 0x20,
    0x06,
    0x06,
    0x20,
    0x38,
    0x06,
    0x20,
    0x0d,
    0x06,
    0x20,
    0x2a,
    0x06,
    0x60,
    0xa9,
    0x02,
    0x85,
    0x02,
    0xa9,
    0x04,
    0x85,
    0x03,
    0xa9,
    0x11,
    0x85,
    0x10,
    0xa9,
    0x10,
    0x85,
    0x12,
    0xa9,
    0x0f,
    0x85,
    0x14,
    0xa9,
    0x04,
    0x85,
    0x11,
    0x85,
    0x13,
    0x85,
    0x15,
    0x60,
    0xa5,
    0xfe,
    0x85,
    0x00,
    0xa5,
    0xfe,
    0x29,
    0x03,
    0x18,
    0x69,
    0x02,
    0x85,
    0x01,
    0x60,
    0x20,
    0x4d,
    0x06,
    0x20,
    0x8d,
    0x06,
    0x20,
    0xc3,
    0x06,
    0x20,
    0x19,
    0x07,
    0x20,
    0x20,
    0x07,
    0x20,
    0x2d,
    0x07,
    0x4c,
    0x38,
    0x06,
    0xa5,
    0xff,
    0xc9,
    0x77,
    0xf0,
    0x0d,
    0xc9,
    0x64,
    0xf0,
    0x14,
    0xc9,
    0x73,
    0xf0,
    0x1b,
    0xc9,
    0x61,
    0xf0,
    0x22,
    0x60,
    0xa9,
    0x04,
    0x24,
    0x02,
    0xd0,
    0x26,
    0xa9,
    0x01,
    0x85,
    0x02,
    0x60,
    0xa9,
    0x08,
    0x24,
    0x02,
    0xd0,
    0x1b,
    0xa9,
    0x02,
    0x85,
    0x02,
    0x60,
    0xa9,
    0x01,
    0x24,
    0x02,
    0xd0,
    0x10,
    0xa9,
    0x04,
    0x85,
    0x02,
    0x60,
    0xa9,
    0x02,
    0x24,
    0x02,
    0xd0,
    0x05,
    0xa9,
    0x08,
    0x85,
    0x02,
    0x60,
    0x60,
    0x20,
    0x94,
    0x06,
    0x20,
    0xa8,
    0x06,
    0x60,
    0xa5,
    0x00,
    0xc5,
    0x10,
    0xd0,
    0x0d,
    0xa5,
    0x01,
    0xc5,
    0x11,
    0xd0,
    0x07,
    0xe6,
    0x03,
    0xe6,
    0x03,
    0x20,
    0x2a,
    0x06,
    0x60,
    0xa2,
    0x02,
    0xb5,
    0x10,
    0xc5,
    0x10,
    0xd0,
    0x06,
    0xb5,
    0x11,
    0xc5,
    0x11,
    0xf0,
    0x09,
    0xe8,
    0xe8,
    0xe4,
    0x03,
    0xf0,
    0x06,
    0x4c,
    0xaa,
    0x06,
    0x4c,
    0x35,
    0x07,
    0x60,
    0xa6,
    0x03,
    0xca,
    0x8a,
    0xb5,
    0x10,
    0x95,
    0x12,
    0xca,
    0x10,
    0xf9,
    0xa5,
    0x02,
    0x4a,
    0xb0,
    0x09,
    0x4a,
    0xb0,
    0x19,
    0x4a,
    0xb0,
    0x1f,
    0x4a,
    0xb0,
    0x2f,
    0xa5,
    0x10,
    0x38,
    0xe9,
    0x20,
    0x85,
    0x10,
    0x90,
    0x01,
    0x60,
    0xc6,
    0x11,
    0xa9,
    0x01,
    0xc5,
    0x11,
    0xf0,
    0x28,
    0x60,
    0xe6,
    0x10,
    0xa9,
    0x1f,
    0x24,
    0x10,
    0xf0,
    0x1f,
    0x60,
    0xa5,
    0x10,
    0x18,
    0x69,
    0x20,
    0x85,
    0x10,
    0xb0,
    0x01,
    0x60,
    0xe6,
    0x11,
    0xa9,
    0x06,
    0xc5,
    0x11,
    0xf0,
    0x0c,
    0x60,
    0xc6,
    0x10,
    0xa5,
    0x10,
    0x29,
    0x1f,
    0xc9,
    0x1f,
    0xf0,
    0x01,
    0x60,
    0x4c,
    0x35,
    0x07,
    0xa0,
    0x00,
    0xa5,
    0xfe,
    0x91,
    0x00,
    0x60,
    0xa6,
    0x03,
    0xa9,
    0x00,
    0x81,
    0x10,
    0xa2,
    0x00,
    0xa9,
    0x01,
    0x81,
    0x10,
    0x60,
    0xa2,
    0x00,
    0xea,
    0xea,
    0xca,
    0xd0,
    0xfb,
    0x60
  ]

gameCodeAddrSpace :: L.Lens' Emulator BS.ByteString
gameCodeAddrSpace = #memory . memoryRange (gameCodeAddress, gameCodeAddress + gameCodeLength)
  where
    gameCodeLength = toWord16 $ BS.length gameCode

rngAddr :: L.Lens' Emulator Word8
rngAddr = #memory . memoryIx 0xFE

inputAddr :: L.Lens' Emulator Word8
inputAddr = #memory . memoryIx 0xFF

screenDataAddrSpace :: L.Lens' Emulator BS.ByteString
screenDataAddrSpace = #memory . memoryRange (0x200, 0x600)

loggerName :: String
loggerName = "snakeGame"

colorBlack :: SDL.Color
colorBlack = SDL.Color 0 0 0 255

colorWhite :: SDL.Color
colorWhite = SDL.Color 255 255 255 255

colorGray :: SDL.Color
colorGray = SDL.Color 128 128 128 255

colorRed :: SDL.Color
colorRed = SDL.Color 255 0 0 255

colorGreen :: SDL.Color
colorGreen = SDL.Color 0 255 0 255

colorBlue :: SDL.Color
colorBlue = SDL.Color 0 0 255 255

colorMagenta :: SDL.Color
colorMagenta = SDL.Color 255 0 255 255

colorYellow :: SDL.Color
colorYellow = SDL.Color 255 255 0 255

colorCyan :: SDL.Color
colorCyan = SDL.Color 0 255 255 255

mapColor :: Word8 -> SDL.Color
mapColor 0 = colorBlack
mapColor 1 = colorWhite
mapColor 2 = colorGray
mapColor 3 = colorRed
mapColor 4 = colorGreen
mapColor 5 = colorBlue
mapColor 6 = colorMagenta
mapColor 7 = colorYellow
mapColor 9 = colorGray
mapColor 10 = colorRed
mapColor 11 = colorGreen
mapColor 12 = colorBlue
mapColor 13 = colorMagenta
mapColor 14 = colorYellow
mapColor _ = colorCyan

data Flag = LogDebug | LogFile String | AlsoLogToStderr deriving (Show, Eq)

options :: [OptDescr Flag]
options =
  [ Option ['d'] ["logdebug"] (NoArg LogDebug) "enable debug logging",
    Option ['f'] ["logfile"] (ReqArg LogFile "FILENAME") "input FILE",
    Option [] ["alsologtostderr"] (NoArg AlsoLogToStderr) "Also output logs to stderr"
  ]

getLogFileFromFlags :: [Flag] -> Maybe String
getLogFileFromFlags (LogFile s : _) = Just s
getLogFileFromFlags (_ : fs) = getLogFileFromFlags fs
getLogFileFromFlags _ = Nothing

parseArgs :: IO [Flag]
parseArgs = do
  argv <- getArgs
  progName <- getProgName
  let header = "Usage: " ++ progName ++ " [OPTION...]"
  case getOpt Permute options argv of
    (o, [], []) -> return o
    (_, _, errs) | not (null errs) -> ioError (userError (concat errs ++ usageInfo header options))
    (_, n, _) -> ioError (userError ("Unexpected non-flags provided: " ++ concat n ++ usageInfo header options))

snakeGame :: IO ()
snakeGame = do
  flags <- parseArgs
  logger <- setupLogging flags
  (window, renderer, texture) <- setupSDL
  let emulator = newEmulator & gameCodeAddrSpace .~ gameCode
  let screenData = ScreenData $ BS.replicate (32 * 3) 0
  stdGen <- initStdGen
  gameLoop stdGen logger renderer texture emulator screenData
  SDL.destroyWindow window
  Log.removeAllHandlers

setupLogging :: [Flag] -> IO Logger
setupLogging flags = do
  unless (AlsoLogToStderr `elem` flags) $ updateGlobalLogger rootLoggerName Log.removeHandler
  t <- getPOSIXTime
  when (LogDebug `elem` flags) $ updateGlobalLogger loggerName (Log.setLevel DEBUG)
  let logFile = fromMaybe ("snakeGame" ++ show t ++ ".log") $ getLogFileFromFlags flags
  h <- Log.fileHandler logFile DEBUG
  updateGlobalLogger loggerName $ Log.addHandler h
  getLogger loggerName

setupSDL :: IO (SDL.Window, SDL.Renderer, SDL.Texture)
setupSDL = do
  SDL.initializeAll
  window <-
    SDL.createWindow
      "Snake Game"
      SDL.defaultWindow
        { SDL.windowPosition = SDL.Centered,
          SDL.windowInitialSize = V2 320 320
        }
  let rendererConfig =
        SDL.RendererConfig
          { rendererType = SDL.AcceleratedVSyncRenderer,
            rendererTargetTexture = True
          }
  renderer <- SDL.createRenderer window (-1) rendererConfig
  SDL.rendererScale renderer $= V2 10 10
  texture <- SDL.createTexture renderer SDL.RGB24 SDL.TextureAccessTarget (V2 32 32)
  return (window, renderer, texture)

data InputData = InputData
  { shouldQuit :: Bool,
    keypress :: Maybe Word8
  }

newtype ScreenData = ScreenData BS.ByteString deriving (Show, Eq)

gameLoop :: StdGen -> Logger -> SDL.Renderer -> SDL.Texture -> Emulator -> ScreenData -> IO ()
gameLoop stdGen logger renderer texture emulator screenData = do
  events <- SDL.pollEvents
  let (InputData shouldQuit keypress) = foldl' processEvent (InputData False Nothing) events
  let screenData' = getScreenData emulator
  when (screenData' /= screenData) $ updateScreen screenData' renderer texture
  let (rand, stdGen') = uniformR (1, 16) stdGen
  (instruction, emulator', ()) <- flip3 runRWST emulator () $ do
    inputAddr %= flip fromMaybe keypress
    rngAddr .= rand
    stepEmulator
  let shouldLog = Log.getLevel logger == Just DEBUG && not (isNop instruction)
  when shouldLog $ logEmulatorStep logger instruction emulator'
  threadDelay 70
  let exitGame = shouldQuit || isBrk instruction
  unless exitGame $ gameLoop stdGen' logger renderer texture emulator' screenData'

processEvent :: InputData -> SDL.Event -> InputData
processEvent input event = case SDL.eventPayload event of
  SDL.QuitEvent -> input {shouldQuit = True}
  SDL.KeyboardEvent (SDL.KeyboardEventData {keyboardEventKeyMotion = SDL.Pressed, keyboardEventKeysym = keysym}) ->
    case SDL.keysymKeycode keysym of
      SDL.KeycodeEscape -> input {shouldQuit = True}
      SDL.KeycodeW -> input {keypress = Just 0x77}
      SDL.KeycodeS -> input {keypress = Just 0x73}
      SDL.KeycodeA -> input {keypress = Just 0x61}
      SDL.KeycodeD -> input {keypress = Just 0x64}
      _ -> input
  _ -> input

getScreenData :: Emulator -> ScreenData
getScreenData = L.views screenDataAddrSpace ScreenData

updateScreen :: ScreenData -> SDL.Renderer -> SDL.Texture -> IO ()
updateScreen (ScreenData sd) renderer texture = do
  let textureData = sd & BS.unpack <&> mapColor
  SDL.updateTexture texture Nothing (encodeTextureData textureData) (32 * 3)
  SDL.copy renderer texture Nothing Nothing
  SDL.present renderer

encodeTextureData :: [SDL.Color] -> BS.ByteString
encodeTextureData = foldMap' renderColor >>> BS.toLazyByteString >>> BS.toStrict
  where
    renderColor (SDL.Color r g b _) = BS.word8 r <> BS.word8 g <> BS.word8 b

logEmulatorStep :: Logger -> Instruction -> Emulator -> IO ()
logEmulatorStep logger instruction emulator = do
  let logDebug = logL logger DEBUG
  logDebug $ show instruction
  logDebug $ show $ emulator ^. #cpu
  logDebug $ "Apple location: " ++ showScreenLocation (emulator ^. #memory . readWord16 0x0)
  logDebug $ "Snake head location: " ++ showScreenLocation (emulator ^. #memory . readWord16 0x10)
  logDebug $ "Snake body 1 location: " ++ showScreenLocation (emulator ^. #memory . readWord16 0x12)
  logDebug $ "Snake body 2 location: " ++ showScreenLocation (emulator ^. #memory . readWord16 0x14)
  logDebug $ "Snake body 3 location: " ++ showScreenLocation (emulator ^. #memory . readWord16 0x16)
  logDebug $ "Snake direction: " ++ show (emulator ^. #memory . memoryIx 0x2)
  logDebug $ "Snake length in bytes: " ++ show (emulator ^. #memory . memoryIx 0x3)

showScreenLocation :: Word16 -> String
showScreenLocation val = "stripLocation: " ++ show lo ++ ", stripNum: " ++ show hi
  where
    (lo, hi) = val ^. L.from packWord16

isBrk :: Instruction -> Bool
isBrk (Instruction BRK _ _) = True
isBrk _ = False

isNop :: Instruction -> Bool
isNop (Instruction NOP _ _) = True
isNop _ = False