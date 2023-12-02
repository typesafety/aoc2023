module Solutions.Day01 where

import MyPrelude

import Control.Arrow ((&&&))
import Data.Char (isDigit)
import Data.List qualified as List
import Text.Read (readMaybe)

import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as Lex

-- | Solve part 1.
solve1 :: Text -> Text
solve1 = showt . sum . fmap calibrateValue . lines . unpackt

calibrateValue :: [Char] -> Int
calibrateValue chars =
    let (d1, d2) = (partialHead &&& partialLast) (List.filter isDigit chars)
    in partialFromJust $ readMaybe @Int [d1, d2]

-- | Solve part 2.
solve2 :: Text -> Text
solve2 =
    showt
    . sum
    . fmap (partialParse @String @Int Lex.decimal . firstAndLast)
    . partialParse input2P
  where
    firstAndLast :: Seq Char -> [Char]
    firstAndLast chars = case (chars, chars) of
        (d1 :<| _, _ :|> dn) -> [d1, dn]
        _ -> error "No digits found on line."

type Parser = P.Parsec Void Text

input2P :: Parser [Seq Char]
input2P = P.someTill lineTokensP P.eof

lineTokensP :: Parser (Seq Char)
lineTokensP = atEol >>= \case
    True -> pure []
    False -> peekTokenP <* P.anySingle >>= \case
        Just token -> (token <|) <$> lineTokensP
        Nothing -> lineTokensP

-- Note: Does not consume any input.
peekTokenP :: Parser (Maybe Char)
peekTokenP = P.optional $ foldl' (<|>) empty (P.lookAhead <$> digitParsers)

digitParsers :: [Parser Char]
digitParsers = [
    '1' <$ "one",
    '2' <$ "two",
    '3' <$ "three",
    '4' <$ "four",
    '5' <$ "five",
    '6' <$ "six",
    '7' <$ "seven",
    '8' <$ "eight",
    '9' <$ "nine",
    '0' <$ "zero"
    ] <> [
        P.digitChar
    ]
