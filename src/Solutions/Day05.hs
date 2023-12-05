{-# language DeriveGeneric #-}
{-# language DuplicateRecordFields #-}
{-# language NoFieldSelectors #-}
{-# language NoImplicitPrelude #-}
{-# language OverloadedLabels #-}
{-# language OverloadedStrings #-}
{-# language PackageImports #-}

module Solutions.Day05 where

import MyPrelude

import "base" Data.Foldable (asum)

import "megaparsec" Text.Megaparsec qualified as P
import "megaparsec" Text.Megaparsec.Char qualified as P
import "megaparsec" Text.Megaparsec.Char.Lexer qualified as Lex
import "optics" Optics hiding (mapping)

-- * Part 1

solve1 :: Text -> Text
solve1 = showt . minimum . seedLocations . partialParse inputP

seedLocations :: Almanac -> [Int]
seedLocations almanac = fmap (mapsFunc (almanac ^. #maps)) (almanac ^. #seeds)

-- | Compose the 'mapFunc' functions for a given list of 'Map's, such that the
-- first Map in the list will run first, and the last Map will run last.
mapsFunc :: [Map] -> Int -> Int
mapsFunc = foldl' (\accFunc m -> mapFunc m . accFunc) identity

-- | Given a 'Map', return a function that transform an input ID according to
-- the mapping rules.  For example, the function given by a "water-to-light"
-- Map, applied to some ID, would return the associated ID to input into the
-- "light-to-temperature" Map.
mapFunc :: Map -> Int -> Int
mapFunc m n = fromMaybe n . asum . fmap (($ n) . mappingFunc) $ (m  ^. #mappings)

-- | Given a 'Mapping', return a function that calculates the new ID for a given
-- input, or 'Nothing' if the input is not in range for the mapping.
mappingFunc :: Mapping -> Int -> Maybe Int
mappingFunc mapping id
    | inRange id mapping = Just (id + (mapping ^. #destinationStart - mapping ^. #sourceStart))
    | otherwise = Nothing
  where
    inRange :: Int -> Mapping -> Bool
    inRange n m =
        let rangeMin = m ^. #sourceStart
            rangeMax = (m ^. #sourceStart) + (m ^. #length) - 1
        in n >= rangeMin && n <= rangeMax

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
    destinationStart :: Int,
    sourceStart :: Int,
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
    destStart <- Lex.decimal
    _ <- P.hspace1
    sourceStart <- Lex.decimal
    _ <- P.hspace1
    len <- Lex.decimal

    pure (Mapping destStart sourceStart len)

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
