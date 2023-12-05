{-# language DeriveGeneric #-}
{-# language DuplicateRecordFields #-}
{-# language LambdaCase #-}
{-# language NoFieldSelectors #-}
{-# language NoImplicitPrelude #-}
{-# language OverloadedLabels #-}
{-# language OverloadedLists #-}
{-# language OverloadedStrings #-}
{-# language PackageImports #-}

module Solutions.Day05 where

import MyPrelude

import "megaparsec" Text.Megaparsec qualified as P
import "megaparsec" Text.Megaparsec.Char qualified as P
import "megaparsec" Text.Megaparsec.Char.Lexer qualified as Lex

-- * Part 1

solve1 :: Text -> Text
solve1 = todo

data Almanac = Almanac {
    seeds :: [Int],
    maps :: [Map]
    }
    deriving (Eq, Show, Generic)

data Map = Map {
    mapType :: (Cat, Cat),
    mappings :: [Mapping]
    }
    deriving (Eq, Show, Generic)

data Cat
    = Seed
    | Soil
    | Fertilizer
    | Water
    | Light
    | Temperature
    | Humidity
    | Location
    deriving (Eq, Show)

data Mapping = Mapping {
    source :: Int,
    destination :: Int,
    length :: Int
    }
    deriving (Eq, Show, Generic)

-- ** Parsing

type Parser = P.Parsec Void Text

inputP :: Parser Almanac
inputP = do
    seeds <- seedsP
    _ <- P.eol
    maps <- P.sepBy mapP P.eol <* P.eof
    pure (Almanac seeds maps)

seedsP :: Parser [Int]
seedsP = P.string "seeds:" *> P.hspace *> P.sepBy Lex.decimal P.hspace <* P.eol

mapP :: Parser Map
mapP = do
    -- Parse header
    sourceCat <- catP
    _ <- P.string "-to-"
    destCat <- catP
    _ <- P.hspace *> P.string "map:" *> P.eol

    -- Parse the lines of mappings
    mappings <- P.sepEndBy mappingP P.eol

    pure (Map (sourceCat, destCat) mappings)

mappingP :: Parser Mapping
mappingP = do
    sourceStart <- Lex.decimal
    _ <- P.hspace1
    destStart <- Lex.decimal
    _ <- P.hspace1
    len <- Lex.decimal

    pure (Mapping sourceStart destStart len)


catP :: Parser Cat
catP = "seed" $> Seed
    <|> "soil" $> Soil
    <|> "fertilizer" $> Fertilizer
    <|> "water" $> Water
    <|> "light" $> Light
    <|> "temperature" $> Temperature
    <|> "humidity" $> Humidity
    <|> "location" $> Location

-- * Part 2

solve2 :: Text -> Text
solve2 = todo
