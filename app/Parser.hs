{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Parser where

import Control.Monad.State
import Data.Bits
import qualified Data.ByteString.Lazy as B
import Data.Functor
import Data.Int (Int32, Int8)
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import Data.Void
import Data.Word
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Byte
import Text.Megaparsec.Byte.Binary

type Parser = Parsec Void B.ByteString
type BitDepth = Int8
type RGB = (Word8, Word8, Word8)
type Palette = M.Map Int RGB

data ColourType = Greyscale Bool | Truecolour Bool | IndexedColour
    deriving (Show)
data ImageType = ImageType {colourType :: ColourType, bitDepth :: BitDepth}
data ImageHeader = ImageHeader {width :: Word32, height :: Word32, imageType :: ImageType, interlace :: Bool}
data PNGImage = PNGImage {header :: ImageHeader, palette :: Maybe Palette}

pPNGBytestream :: Parser PNGImage
pPNGBytestream = do
    pngSignature
    header <- pImageHeader
    palette <- optional $ pPalette header
    pure $ PNGImage{..}
  where
    pngSignature = string (B.pack [137, 80, 78, 71, 13, 10, 26, 10]) <?> "png signature"

pImageHeader :: Parser ImageHeader
pImageHeader = pChunk "IHDR" $ const pImageHeaderData

pPalette :: ImageHeader -> Parser Palette
pPalette = pChunk "PLTE" . pPaletteData

pChunk :: B.ByteString -> (Word32 -> Parser a) -> Parser a
pChunk chunkName pData = do
    chunkLength <- word32be
    string chunkName
    -- discard cyclic redundancy check, assuming data isn't corrupt for now
    pData chunkLength <* count 4 anySingle

-- TODO: add support for 1, 2, 4 bit depths
--
-- note that this method of converting 16 bit depth to 8 bit depth is not as accurate
-- as one proposed in the PNG spec:
-- output = floor((input * MAXOUTSAMPLE / MAXINSAMPLE) + 0.5)
-- but it's pretty damn close.
pSample :: BitDepth -> Parser Word8
pSample bitDepth = do
    case bitDepth of
        8 -> word8
        16 -> fromIntegral . (.>>. 8) <$> word16be
        _ -> fail $ "Unsupported bit depth: " ++ show bitDepth

pPaletteData :: ImageHeader -> Word32 -> Parser Palette
pPaletteData hdr n = M.fromList . zip [1 ..] <$> count (fromIntegral n `div` 3) pixel
  where
    depth = (bitDepth . imageType) hdr
    sample = pSample depth
    pixel = (,,) <$> sample <*> sample <*> sample

pImageHeaderData :: Parser ImageHeader
pImageHeaderData = do
    ImageHeader
        <$> word32be -- width
        <*> word32be -- heigth
        <*> pImageType
        <* (char 0 <?> "compression method")
        <* (char 0 <?> "filter method")
        <*> (pBool <?> "interlace method")

pBool :: Parser Bool
pBool = (== 1) <$> (char 0 <|> char 1) <?> "bool (0 or 1)"

pImageType :: Parser ImageType
pImageType = do
    bitDepth <- parseDepth
    colourType <- pColourType
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
parseDepth = fromIntegral <$> oneOf [1, 2, 4, 8, 16]

-- TODO: get better error message by making this function take depth as input
pColourType :: Parser ColourType
pColourType =
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

check :: Parser Bool -> Parser a -> Parser a
check condition parser = do
    result <- lookAhead condition
    if result
        then parser
        else fail "Check failed"
