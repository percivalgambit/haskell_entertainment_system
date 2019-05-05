module BitGet (
    BitGet,
    runBitGet,

    skip,
    remaining,
    isEmpty,
    lookAhead,

    getBit,
    getByteString,

    getAsWord8,
    getAsWord16,
    getAsWord32
) where

import           Util                (decodeWord16, decodeWord32)

import           Control.Monad.State (State, evalState, get, put)
import           Data.Bits           (complement, shiftL, shiftR, zeroBits,
                                      (.&.), (.|.))
import qualified Data.ByteString     as B
import           Data.Word           (Word16, Word32, Word8)

data BitString = BitString B.ByteString Word8

type BitGet = State BitString

runBitGet :: B.ByteString -> BitGet a -> a
runBitGet input bg = evalState bg $ BitString input 0

readN :: Int -> (B.ByteString -> a) -> BitGet a
readN nBits f = do
    BitString bs off <- get
    let offset = fromIntegral off :: Int
    let bitsRemaining = fromIntegral $ B.length bs * 8 - offset
    if bitsRemaining < nBits
        then error $ "Tried to read " ++ show nBits ++ " bits, but only " ++ show bitsRemaining ++ "remained"
        else do
            let bytesRequired = ((nBits - 1 + offset) `div` 8) + 1
            let offset' = (offset + nBits) `mod` 8
            let (r, rest) = if offset' == 0
                    then B.splitAt bytesRequired bs
                    else splitAtWithDupByte bytesRequired bs
            put $ BitString rest $ fromIntegral offset'
            return $ f $ truncateBits nBits $ shiftBits offset r
  where
    splitAtWithDupByte n bs = (B.take n bs, B.drop (n - 1) bs)
    shiftBits n = snd . B.mapAccumR (shiftAccum n) 0
    shiftAccum n acc b = (b `shiftR` (8 - n), (b `shiftL` n) .|. acc)
    truncateBits n = B.take ((n + 7) `div` 8) . snd . B.mapAccumL truncateAccum n
    truncateAccum bits w | bits >= 8 = (bits - 8, w)
                         | bits == 0 = (0, 0)
                         | otherwise = (0, w .&. topNBits bits)
    topNBits b = complement zeroBits `shiftL` (8 - b)


skip :: Int -> BitGet ()
skip n = readN n $ const ()

remaining :: BitGet Int
remaining = do
  BitString bs offset <- get
  return $ B.length bs * 8 - fromIntegral offset

isEmpty :: BitGet Bool
isEmpty = do
    BitString bs _ <- get
    return $ B.null bs

lookAhead :: BitGet a -> BitGet a
lookAhead bg = do
    s <- get
    a <- bg
    put s
    return a

getBit :: BitGet Bool
getBit = readN 1 (not . (==) 0 . B.head)

getByteString :: Int -> BitGet B.ByteString
getByteString n = readN n id

getAsWord8 :: Int -> BitGet Word8
getAsWord8 n = readN n $ flip B.index 0

getAsWord16 :: Int -> BitGet Word16
getAsWord16 n = readN n $ \b -> decodeWord16 (B.index b 0) (B.index b 1)

getAsWord32 :: Int -> BitGet Word32
getAsWord32 n = readN n $ \b ->
    decodeWord32 (B.index b 0) (B.index b 1) (B.index b 2) (B.index b 3)
