{-# language DeriveGeneric #-}
{-# language DuplicateRecordFields #-}
{-# language NoFieldSelectors #-}
{-# language NoImplicitPrelude #-}
{-# language OverloadedLabels #-}
{-# language OverloadedLists #-}
{-# language OverloadedStrings #-}
{-# language PackageImports #-}

module Solutions.Day06 where

import MyPrelude

import "base" Data.List qualified as List

import "megaparsec" Text.Megaparsec qualified as P
import "megaparsec" Text.Megaparsec.Char qualified as P
import "megaparsec" Text.Megaparsec.Char.Lexer qualified as Lex
import "optics" Optics

-- * Part 1

solve1 :: Text -> Text
solve1 = showt
    . product
    . fmap (List.length . winningHoldTimes)
    . partialParse inputP

-- | Return a list of integers for which holding the button results in a win,
-- for the given race.
winningHoldTimes :: Race -> [Int]
winningHoldTimes race = [
    holdTime |
    holdTime <- [1 .. race ^. #time - 1],
    calcDistance (race ^. #time) holdTime > race ^. #distance
    ]

-- | Calculate the distance travelled during a race if the button is held for x
-- milliseconds and the time allowed in the race is y milliseconds.
calcDistance ::
    -- | Time allowed by the race.
    Int ->
    -- | Time to hold the button.
    Int ->
    -- | Distance traveled.
    Int
calcDistance maxTime holdTime = max (holdTime * (maxTime - holdTime)) 0

data Race = Race {
    time :: Int,
    distance :: Int
    }
    deriving (Eq, Show, Generic)

-- ** Parser

type Parser = P.Parsec Void Text

inputP :: Parser [Race]
inputP = do
    times <- P.string "Time:" *> P.hspace *> P.sepBy Lex.decimal P.hspace1 <* P.eol
    distances <- P.string "Distance:" *> P.hspace *> P.sepBy Lex.decimal P.hspace1 <* P.eol
    pure $ fmap (uncurry Race) (List.zip times distances)

-- * Part 2

solve2 :: Text -> Text
solve2 = todo
