{-# LANGUAGE RecordWildCards #-}

module Parser where

import Data.Bits
import qualified Data.ByteString.Lazy as B
import Data.Int (Int32, Int8)
import Data.Void
import Data.Word
import Text.Megaparsec
import Text.Megaparsec.Byte

type Parser = Parsec Void B.ByteString
type BitDepth = Int8

data ColourType = Greyscale Bool | Truecolour Bool | IndexedColour
    deriving (Show)
data ImageType = ImageType {colourType :: ColourType, bitDepth :: BitDepth}
data ImageHeader = ImageHeader {width :: Int32, height :: Int32, imageType :: ImageType, interlace :: Bool}
data Chunk = IHDR ImageHeader | PLTE

-- nthBit :: Int -> Word8 -> Bool
-- nthBit n word = (word .>>. n) .&. 1 == 1

parseImageType :: Parser ImageType
parseImageType = do
    bitDepth <- toEnum . fromEnum <$> anySingle
    colourType <- oneOf [0, 2, 3, 4, 6] >>= byteToColourType
    if allowedImageType colourType bitDepth
        then pure ImageType{..}
        else
            fail
                $ "invalid (colour, bit depth): ("
                ++ show colourType
                ++ ","
                ++ show bitDepth
                ++ ")"

allowedImageType :: ColourType -> BitDepth -> Bool
allowedImageType (Greyscale False) = flip elem [1, 2, 4, 8, 16]
allowedImageType IndexedColour = flip elem [1, 2, 4, 8]
allowedImageType _ = flip elem [8, 16]

byteToColourType :: Word8 -> Parser ColourType
byteToColourType 0 = pure $ Greyscale False
byteToColourType 2 = pure $ Truecolour False
byteToColourType 3 = pure IndexedColour
byteToColourType 4 = pure $ Greyscale True
byteToColourType 6 = pure $ Truecolour True
byteToColourType b = fail $ "expected one of [0, 2, 3, 4, 6], got " ++ show b

-- byteToColourType c
--     | not (b0 || b1 || b2) = Just $ Greyscale False
--     | not b0 && b1 && not b2 = Just $ Truecolour False
--     | not b0 && b1 && b2 = Just IndexedColour
--     | b0 && not b1 && not b2 = Just $ Greyscale True
--     | b0 && b1 && not b2 = Just $ Truecolour True
--     | otherwise = Nothing
--   where
--     (b0, b1, b2) = (nthBit 0 c, nthBit 1 c, nthBit 2 c)
