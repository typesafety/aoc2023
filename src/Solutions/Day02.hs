{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedLabels #-}

module Solutions.Day02 where

import MyPrelude

import Data.List qualified as List

import Optics
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as Lex

-- * Part 1

solve1 :: Text -> Text
solve1 = showt
    . sum
    . fmap (view #id)
    . List.filter gameValid
    . partialParse inputP
  where
    gameValid :: Game -> Bool
    gameValid = all cubesValid . view #cubeReveals
      where
        cubesValid :: [(Int, Color)] -> Bool
        cubesValid = all cubeRevealValid
          where
            cubeRevealValid :: (Int, Color) -> Bool
            cubeRevealValid (n, color) =
                let condition = case color of
                        Red -> (<= 12)
                        Green -> (<= 13)
                        Blue -> (<= 14)
                in condition n

data Game = Game {
    id :: Int,
    cubeReveals :: [[(Int, Color)]]
    }
    deriving (Eq, Show, Generic)

data Color = Red | Green | Blue
    deriving (Eq, Show)

-- * Parsing

type Parser = P.Parsec Void Text

inputP :: Parser [Game]
inputP = gameP `P.sepEndBy` P.eol
  where
    gameP :: Parser Game
    gameP = do
        id <- P.string "Game" *> P.hspace1 *> Lex.decimal <* P.char ':' <* P.hspace
        cubes <- cubeSetP `P.sepBy` (P.char ';' *> P.hspace)
        pure (Game id cubes)

    cubeSetP :: Parser [(Int, Color)]
    cubeSetP = cubeP `P.sepBy` (P.char ',' *> P.hspace)

    cubeP :: Parser (Int, Color)
    cubeP = do
        count <- Lex.decimal
        _ <- P.hspace1
        color <- colorP
        pure (count, color)

    colorP :: Parser Color
    colorP =
        Red <$ "red"
        <|> Green <$ "green"
        <|> Blue <$ "blue"

-- * Part 2

solve2 :: Text -> Text
solve2 = todo
