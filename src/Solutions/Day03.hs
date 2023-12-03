{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedLabels #-}

module Solutions.Day03 where

import MyPrelude

import Data.Maybe (mapMaybe)
import Data.List qualified as List

import Data.HashMap.Strict qualified as HM
import Optics (
    (%),
    (^.),
    over,
    Field1(_1),
    Field2(_2)
    )
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as Lex

-- * Part 1

solve1 :: Text -> Text
solve1 = showt
    . foldl' (\acc partNumber -> partNumber ^. #number + acc) 0
    . getPartNumbers
    . parseSchematic . partialParse inputP

data PartNumber = PartNumber {
    number :: Int,
    symbol :: Char,
    symbolPoint :: Point  -- Location of the symbol that this part is attached to.
    }
    deriving (Eq, Show, Generic)

getPartNumbers :: Schematic -> [PartNumber]
getPartNumbers schem = mapMaybe (getPartNumber (schem ^. #symbols)) (schem ^. #numbers)

getPartNumber :: HashMap Point Char -> Number -> Maybe PartNumber
getPartNumber symbols number = do
    (point, char) <- find (\(p, _) -> pointIsAdjacent p number) (HM.toList symbols)
    pure (PartNumber (number ^. #value) char point)

pointIsAdjacent :: Point -> Number -> Bool
pointIsAdjacent (px, py) number =
    px >= minX
    && px <= maxX
    && py >= minY
    && py <= maxY
  where
    maxX, maxY, minX, minY :: Int
    maxX = number ^. #end % _1 + 1
    maxY = number ^. #end % _2 + 1
    minX = number ^. #start % _1 - 1
    minY = number ^. #start % _2 - 1

-- | X/Y coordinates, top left being (0, 0).
type Point = (Int, Int)

data Number = Number {
    start :: Point,
    end :: Point,
    value :: Int
    }
    deriving (Eq, Show, Generic)

data Schematic = Schematic {
    symbols :: HashMap Point Char,
    numbers :: [Number]
    }
    deriving (Eq, Show, Generic)

parseSchematic :: [[Char]] -> Schematic
parseSchematic matrix = go 0 matrix (Schematic [] [])
  where
    go :: Int -> [[Char]] -> Schematic -> Schematic
    go _ [] schem = schem
    go y (row : rows) schem = go (y + 1) rows (parseRow y row schem)

    parseRow :: Int -> [Char] -> Schematic -> Schematic
    parseRow = go2 Nothing 0
      where
        go2 :: Maybe (Int, [Char]) -> Int -> Int -> [Char] -> Schematic -> Schematic
        go2 Nothing _ _ [] schem = schem
        go2 Nothing x y (char : chars) schem
            | isDigit char = go2 (Just (x, [char])) (x + 1) y chars schem
            | char == '.' = go2 Nothing (x + 1) y chars schem
            | otherwise =
                let updatedSchem = over #symbols (<> [((x, y), char)]) schem
                in go2 Nothing (x + 1) y chars updatedSchem
        go2 (Just (start, digitChars)) x y [] schem = addNumber schem
          where
            addNumber :: Schematic -> Schematic
            addNumber = over #numbers (<> [Number (start, y) (x - 1, y) (partialParse Lex.decimal digitChars)])
        go2 (Just (start, digitChars)) x y (char : chars) schem
            | isDigit char = go2 (Just (start, digitChars <> [char])) (x + 1) y chars schem
            | char == '.' = go2 Nothing (x + 1) y chars (addNumber schem)
            | otherwise = go2 Nothing (x + 1) y chars ((addSymbol . addNumber) schem)
          where
            addSymbol :: Schematic -> Schematic
            addSymbol = over #symbols (<> [((x, y), char)])

            addNumber :: Schematic -> Schematic
            addNumber = over #numbers (<> [Number (start, y) (x - 1, y) (partialParse Lex.decimal digitChars)])

-- ** Parsing

type Parser = P.Parsec Void Text

inputP :: Parser [[Char]]
inputP = P.someTill (P.someTill P.anySingle P.eol) P.eof

-- * Part 2

solve2 :: Text -> Text
solve2 = showt
    . HM.foldl' (\acc numbers -> acc + product numbers) 0
    . gears
    . getPartNumbers
    . parseSchematic . partialParse inputP

gears :: [PartNumber] -> HashMap (Point, Char) [Int]
gears =
    HM.filter ((== 2) . List.length)
    . go HM.empty
    . List.filter (\pn -> pn ^. #symbol == '*')
  where
    go :: HashMap (Point, Char) [Int] -> [PartNumber] -> HashMap (Point, Char) [Int]
    go hm [] = hm
    go hm (p : pn) = go (HM.insertWith (<>) (p ^. #symbolPoint, p ^. #symbol) [p ^. #number] hm) pn
