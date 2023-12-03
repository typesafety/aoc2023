{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedLabels #-}

module Solutions.Day02 where

import MyPrelude

import Data.List qualified as List

import Optics (
    view,
    (^.),
    over,
    )
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
    gameValid = all cubeSetValid . view #cubeSets
      where
        cubeSetValid :: CubeSet -> Bool
        cubeSetValid cs = and @[] [
            view #red cs <= 12,
            view #green cs <= 13,
            view #blue cs <= 14
            ]

data Game = Game {
    id :: Int,
    cubeSets :: [CubeSet]
    }
    deriving (Eq, Show, Generic)

data CubeSet = CubeSet {
    red :: Int,
    green :: Int,
    blue :: Int
    }
    deriving (Eq, Show, Generic)

data Color = Red | Green | Blue
    deriving (Eq, Show)

-- ** Parsing

type Parser = P.Parsec Void Text

inputP :: Parser [Game]
inputP = gameP `P.sepEndBy` P.eol
  where
    gameP :: Parser Game
    gameP = do
        id <- P.string "Game" *> P.hspace1 *> Lex.decimal <* P.char ':' <* P.hspace
        cubeSets <- cubeSetP `P.sepBy` (P.char ';' *> P.hspace)
        pure (Game id cubeSets)

    cubeSetP :: Parser CubeSet
    cubeSetP = toCubeSet <$> cubeP `P.sepBy` (P.char ',' *> P.hspace)
      where
        toCubeSet :: [(Int, Color)] -> CubeSet
        toCubeSet = foldl' (flip addToCubeSet) (CubeSet 0 0 0)
          where
            addToCubeSet :: (Int, Color) -> CubeSet -> CubeSet
            addToCubeSet (n, color) cs =
                let modify = case color of
                        Red -> over #red
                        Green -> over #green
                        Blue -> over #blue
                in modify (+ n) cs

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
solve2 =
    showt
    . sum
    . fmap (cubeSetPower . getCubeSet . mconcat . fmap CubeSetMax . view #cubeSets)
    . partialParse inputP
  where
    cubeSetPower :: CubeSet -> Int
    cubeSetPower cs = product @[] [cs ^. #red, cs ^. #green, cs ^. #blue]

-- | Newtype for defining specific semigroup instance.
newtype CubeSetMax = CubeSetMax CubeSet
    deriving (Eq, Show)

getCubeSet :: CubeSetMax -> CubeSet
getCubeSet (CubeSetMax cs) = cs

instance Semigroup CubeSetMax where
    (<>) :: CubeSetMax -> CubeSetMax -> CubeSetMax
    CubeSetMax cs1 <> CubeSetMax cs2 = CubeSetMax $ CubeSet {
        red = max (cs1 ^. #red) (cs2 ^. #red),
        green = max (cs1 ^. #green) (cs2 ^. #green),
        blue = max (cs1 ^. #blue) (cs2 ^. #blue)
        }

instance Monoid CubeSetMax  where
    mempty :: CubeSetMax
    mempty = CubeSetMax (CubeSet 0 0 0)
