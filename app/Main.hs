module Main where

import qualified Data.ByteString.Lazy as B
import Data.Functor
import Parser (Parser, pPNGBytestream)
import Text.Megaparsec (errorBundlePretty, runParser)

main :: IO ()
main = do
    f <- B.readFile "asset.png"
    let res = runParser pPNGBytestream "asset.png" f
    case res of
        Left e -> putStrLn $ errorBundlePretty e
        Right im -> print im
