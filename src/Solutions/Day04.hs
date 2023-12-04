{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedLabels #-}

module Solutions.Day04 where

import MyPrelude

import Data.IntSet qualified as IS
import Optics
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as Lex

-- * Part 1

solve1 :: Text -> Text
solve1 = showt . sum . fmap cardScore . partialParse inputP

cardScore :: Card -> Int
cardScore card =
    let size = IS.size (IS.intersection (card ^. #winningNums) (card ^. #myNums))
    in if size > 0 then 2 ^ (size - 1) else 0

data Card = Card {
    id :: Int,
    winningNums :: IntSet,
    myNums :: IntSet
    }
    deriving (Eq, Show, Generic)

-- ** Parser

type Parser = P.Parsec Void Text

cardP :: Parser Card
cardP = do
    cardId <- P.string "Card" *> P.hspace1 *> Lex.decimal <* P.char ':'
    _ <- P.hspace
    winningNumbers <- P.sepEndBy Lex.decimal P.hspace1
    _ <- P.hspace *> P.char '|' <* P.hspace
    myNumbers <- P.sepBy Lex.decimal P.hspace1
    _ <- P.eol

    pure $ Card cardId (IS.fromList winningNumbers) (IS.fromList myNumbers)

inputP :: Parser [Card]
inputP = P.someTill cardP P.eof

-- * Part 2

solve2 :: Text -> Text
solve2 = todo
