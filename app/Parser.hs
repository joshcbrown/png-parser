{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Parser where

import qualified Codec.Compression.Zlib as Z
import Control.Monad.State
import Crc (crc)
import Data.Bits
import qualified Data.ByteString.Lazy as L
import Data.Functor
import Data.Int (Int32, Int8)
import Data.Maybe (catMaybes)
import qualified Data.Vector as V
import Data.Void
import Data.Word
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Byte
import Text.Megaparsec.Byte.Binary
import Text.Megaparsec.Debug (MonadParsecDbg (dbg))

type Parser = Parsec Void L.ByteString
type BitDepth = Int8
type RGB = (Word8, Word8, Word8)
type Palette = V.Vector RGB

data ColourType = Greyscale Bool | Truecolour Bool | IndexedColour
    deriving (Show)
data ImageType = ImageType {colourType :: ColourType, bitDepth :: BitDepth}
    deriving (Show)
data ImageHeader = ImageHeader {width :: Word32, height :: Word32, imageType :: ImageType, interlace :: Bool}
    deriving (Show)
data PNGImage = PNGImage {header :: ImageHeader, palette :: Maybe Palette, rawData :: L.ByteString}
    deriving (Show)

pPNGBytestream :: Parser PNGImage
pPNGBytestream = do
    pngSignature
    header <- pImageHeader
    many $ try pUnsupported
    palette <- optional $ try $ pPalette header
    rawData <- Z.decompress . L.concat <$> many (try pImageData)
    pImageEnd
    pure $ PNGImage{..}
  where
    pngSignature = string (L.pack [137, 80, 78, 71, 13, 10, 26, 10]) <?> "png signature"

pImageHeader :: Parser ImageHeader
pImageHeader = pChunk "IHDR" $ const pImageHeaderData

pPalette :: ImageHeader -> Parser Palette
pPalette = pChunk "PLTE" . pPaletteData

unsupported :: [L.ByteString]
unsupported =
    [ "tRNS"
    , "cHRM"
    , "gAMA"
    , "iCCP"
    , "sBIT"
    , "sRGB"
    , "cICP"
    , "mDCv"
    , "cLLi"
    , "tEXt"
    , "zTXt"
    , "iTXt"
    , "bKGD"
    , "hIST"
    , "pHYs"
    , "sPLT"
    , "eXlf"
    , "tIME"
    , "acTL"
    , "fcTL"
    , "fdAT"
    ]

pUnsupported :: Parser ()
pUnsupported = pChunk unsupportedName (\length -> count length anySingle $> ())
  where
    unsupportedName = choice $ string <$> unsupported

pImageData :: Parser L.ByteString
pImageData = pChunk "IDAT" pRawData

pImageEnd :: Parser ()
pImageEnd = pChunk "IEND" (const $ pure ())

pChunk :: Parser L.ByteString -> (Int -> Parser a) -> Parser a
pChunk chunkName pData = do
    -- this should be ok as PNG spec says that unsigned 32 bit ints will
    -- never exceed 2^31 - 1
    dataLength <- fromIntegral <$> word32be
    lookAhead $ crcCheck dataLength
    chunkName
    -- discard cyclic redundancy check, already checked before
    pData dataLength <* count 4 anySingle

pRawData :: Int -> Parser L.ByteString
pRawData n = L.pack <$> count n anySingle

crcCheck :: Int -> Parser ()
crcCheck dataLength = do
    let length = 4 + dataLength -- account for chunk name
    chunkData <- L.pack <$> count length anySingle
    expectedCrc <- word32be
    when (crc chunkData /= expectedCrc) $ fail "data corrupt, CRC check failed"

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

pPaletteData :: ImageHeader -> Int -> Parser Palette
pPaletteData hdr dataLength = V.fromListN nPixels <$> count nPixels pixel
  where
    nPixels = dataLength `div` 3
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
