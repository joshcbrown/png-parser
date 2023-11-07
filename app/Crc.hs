module Crc (crc) where

-- CRC check implemented as specified in PNG specification annex D

import Data.Bits
import qualified Data.ByteString.Lazy as B
import Data.List (foldl')
import Data.Vector ((!))
import qualified Data.Vector as V
import Data.Word (Word32, Word64, Word8)

fpow :: Int -> (a -> a) -> a -> a
fpow n = foldl' (.) id . replicate n

crcTable :: V.Vector Word32
crcTable = V.fromListN 256 $ map (fpow 8 cFunc) [0 .. 255]
  where
    cFunc c = if c .&. 1 == 1 then magic .^. (c .>>. 1) else c .>>. 1
    magic = 0xedb88320

updateCrc :: Word32 -> B.ByteString -> Word32
updateCrc = B.foldl' update
  where
    idx c byte = fromIntegral $ (c .^. fromIntegral byte) .&. 0xff
    update c byte = (crcTable ! idx c byte) .^. (c .>>. 8)

crc :: B.ByteString -> Word32
crc = (maxBound .^.) . updateCrc maxBound
