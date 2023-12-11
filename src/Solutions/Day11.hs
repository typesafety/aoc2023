{-# language DuplicateRecordFields #-}
{-# language NoFieldSelectors #-}
{-# language NoImplicitPrelude #-}
{-# language OverloadedLists #-}
{-# language PackageImports #-}

module Solutions.Day11 where

import MyPrelude

import "base" Data.List qualified as List

import "megaparsec" Text.Megaparsec qualified as P
import "megaparsec" Text.Megaparsec.Char qualified as P
import "unordered-containers" Data.HashMap.Strict qualified as HM

-- * Part 1

solve1 :: Text -> Text
solve1 = showt
    . foldl' (\acc pair -> acc + uncurry manhattan pair) 0
    . galaxyPairs
    . toSpaceMap
    . expandImage
    . partialParse inputP

manhattan :: Point -> Point -> Int
manhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

galaxyPairs :: SpaceMap -> [(Point, Point)]
galaxyPairs = pairUp . HM.keys . HM.filter (== Galaxy)
  where
    pairUp :: [a] -> [(a, a)]
    pairUp [] = []
    pairUp (x : xs) = fmap (x, ) xs <> pairUp xs

toSpaceMap :: [[Space]] -> SpaceMap
toSpaceMap = foldl' (<>) [] . List.zipWith go [0 ..]
  where
    go :: Int -> [Space] -> SpaceMap
    go y = HM.fromList . List.zipWith (\x s -> ((x, y), s)) [0 ..]

expandImage :: [[Space]] -> [[Space]]
expandImage = List.transpose . expandHoriz . List.transpose . expandHoriz
  where
    expandHoriz :: [[Space]] -> [[Space]]
    expandHoriz [] = []
    expandHoriz (row : rest)
        | all (== None) row = row : row : expandHoriz rest
        | otherwise = row : expandHoriz rest

type SpaceMap = HashMap Point Space

data Space = None | Galaxy
    deriving (Eq, Show)

type Point = (Int, Int)

-- ** Parsing

type Parser = P.Parsec Void Text

inputP :: Parser [[Space]]
inputP = P.someTill lineP P.eof
  where
    lineP :: Parser [Space]
    lineP = P.someTill pointP P.eol

    pointP :: Parser Space
    pointP = P.char '.' $> None <|> P.char '#' $> Galaxy

-- * Part 2

solve2 :: Text -> Text
solve2 = todo
