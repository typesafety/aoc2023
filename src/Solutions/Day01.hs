module Solutions.Day01 where

import MyPrelude

import Control.Arrow ((&&&))
import Data.Char (isDigit)
import Data.List qualified as List
import Text.Read (readMaybe)

import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as Lex

solve1 :: Text -> Text
solve1 = showt . sum . fmap calibrateValue . lines . unpackt

calibrateValue :: [Char] -> Int
calibrateValue chars =
    let (d1, d2) = (partialHead &&& partialLast) (List.filter isDigit chars)
    in partialFromJust $ readMaybe @Int [d1, d2]

solve2 :: Text -> Text
solve2 = todo

type Parser = P.Parsec Void Text

lineP :: Parser [Int]
lineP = todo

digitTextP :: Parser [Text]
digitTextP = do
    sequenceA [P.lookAhead (P.string "one"), P.lookAhead (P.string "two")]
