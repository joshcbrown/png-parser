{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Parser where

import Data.Bits
import qualified Data.ByteString.Lazy as B
import Data.Functor
import Data.Int (Int32, Int8)
import Data.Void
import Data.Word
import Text.Megaparsec
import Text.Megaparsec.Byte
import Text.Megaparsec.Byte.Binary

type Parser = Parsec Void B.ByteString
type BitDepth = Int8

data ColourType = Greyscale Bool | Truecolour Bool | IndexedColour
    deriving (Show)
data ImageType = ImageType {colourType :: ColourType, bitDepth :: BitDepth}
data ImageHeader = ImageHeader {width :: Word32, height :: Word32, imageType :: ImageType, interlace :: Bool}
data Chunk = IHDR ImageHeader | PLTE

pPNGBytestream :: Parser [Chunk]
pPNGBytestream = string (B.pack [137, 80, 78, 71, 13, 10, 26, 10]) *> many pChunk

pChunk :: Parser Chunk
pChunk = do
    length <- word32be
    choice
        [ string "IHDR" *> (IHDR <$> pImageHeaderData)
        , string "PLTE" $> PLTE -- TODO: fix
        ]
        <* count 4 anySingle -- cyclic redundancy check, assuming data isn't corrupt for now

pImageHeaderData :: Parser ImageHeader
pImageHeaderData =
    ImageHeader
        <$> word32be
        <*> word32be
        <*> parseImageType
        <* count 2 (char 0) -- compression and filter method
        <*> pBool

pBool :: Parser Bool
pBool = (== 1) <$> (char 0 <|> char 1)

parseImageType :: Parser ImageType
parseImageType = do
    bitDepth <- parseDepth
    colourType <- parseColourType
    if allowedImageType colourType bitDepth
        then pure ImageType{..}
        else
            fail
                $ "invalid (colour, bit depth): ("
                ++ show colourType
                ++ ","
                ++ show bitDepth
                ++ ")"

parseDepth :: Parser BitDepth
parseDepth = toEnum . fromEnum <$> oneOf [1, 2, 4, 8, 16]

-- TODO: get better error message by making this function take depth as input
parseColourType :: Parser ColourType
parseColourType =
    choice
        [ char 0 $> Greyscale False
        , char 2 $> Truecolour False
        , char 3 $> IndexedColour
        , char 4 $> Greyscale True
        , char 6 $> Truecolour True
        ]

allowedImageType :: ColourType -> BitDepth -> Bool
allowedImageType (Greyscale False) = flip elem [1, 2, 4, 8, 16]
allowedImageType IndexedColour = flip elem [1, 2, 4, 8]
allowedImageType _ = flip elem [8, 16]
