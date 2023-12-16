{-# language DuplicateRecordFields #-}
{-# language NoFieldSelectors #-}
{-# language NoImplicitPrelude #-}
{-# language OverloadedLabels #-}
{-# language PackageImports #-}

module Solutions.Day14 where

import MyPrelude

import "base" Data.List qualified as List

import "hashable" Data.Hashable (Hashable)
import "megaparsec" Text.Megaparsec qualified as P
import "megaparsec" Text.Megaparsec.Char qualified as P
import "optics" Optics ((^.), over)
import "unordered-containers" Data.HashMap.Strict qualified as HM

-- * Part 1

solve1 :: Text -> Text
solve1 = showt . totalLoad . tiltNorth . toRockMap . partialParse inputP

totalLoad :: RockMap -> Int
totalLoad rockMap =
    HM.foldlWithKey'
        (\total point rock -> total + load point rock rockMap)
        0
        (rockMap ^. #rocks)
  where
    load :: Point -> Maybe Rock -> RockMap -> Int
    load (_, y) r rm = case r of
        Just Round -> (rm ^. #height) - y
        _ -> 0

tiltNorth :: RockMap -> RockMap
tiltNorth rockMap = foldl' tiltColumnNorth rockMap [0 .. rockMap ^. #width - 1]

tiltColumnNorth :: RockMap -> Int -> RockMap
tiltColumnNorth rockMap column =
    fst $
        foldl'
            (\(rm, ps) y -> move ps rm (column, y))
            (rockMap, Empty)
            [0 .. (rockMap ^. #height) - 1]

move :: Seq Point -> RockMap -> Point -> (RockMap, Seq Point)
move vacantPoints rm point = case (rm ^. #rocks) HM.! point of
    Nothing -> (rm, vacantPoints |> point)
    Just Cube -> (rm, Empty)
    Just Round -> case vacantPoints of
        (vp :<| vps) -> (over #rocks (swap point vp) rm, vps |> point)
        Empty -> (rm, Empty)

swap :: (Hashable k, HasCallStack) => k -> k -> HashMap k v -> HashMap k v
swap from to hm =
    let atFrom = hm HM.! from
        atTo = hm HM.! to
    in HM.insert from atTo (HM.insert to atFrom hm)

toRockMap :: [[Maybe Rock]] -> RockMap
toRockMap rocks = RockMap {
        rocks = foldl' (<>) HM.empty . List.zipWith go [0 ..] $ rocks,
        width = length . partialHead $ rocks,
        height = length rocks
    }
  where
    go :: Int -> [Maybe Rock] -> HashMap Point (Maybe Rock)
    go y = HM.fromList . List.zipWith (\x s -> ((x, y), s)) [0 ..]

data RockMap = RockMap {
    rocks :: HashMap Point (Maybe Rock),
    width :: Int,
    height :: Int
    }
    deriving (Eq, Show, Generic)

instance Hashable RockMap

type Point = (Int, Int)

data Rock = Round | Cube
    deriving (Eq, Ord, Show, Generic)

instance Hashable Rock

-- ** Parsing

type Parser = P.Parsec Void Text

inputP :: Parser [[Maybe Rock]]
inputP = P.someTill rowP P.eof

rowP :: Parser [Maybe Rock]
rowP = P.someTill pointP P.eol

pointP :: Parser (Maybe Rock)
pointP = P.char '.' $> Nothing <|> P.char 'O' $> Just Round <|> P.char '#' $> Just Cube

-- * Part 2

solve2 :: Text -> Text
solve2 = showt . totalLoad . tiltCycleN 1_000_000_000 . toRockMap . partialParse inputP

tiltCycleN :: Int -> RockMap -> RockMap
tiltCycleN target rockMap =
    let (diff, n, rm) = findCycle HM.empty 0 rockMap
        remainingCycles = (target - n) `mod` diff
    in applyN remainingCycles tiltCycle rm
  where
    findCycle :: HashMap RockMap (Int, RockMap) -> Int -> RockMap -> (Int, Int, RockMap)
    findCycle memo ix rm = case memo HM.!? rm of
        Just (k, _) -> (ix - k, ix, rm)
        Nothing ->
            let res = tiltCycle rm
            in findCycle (HM.insert rm (ix, res) memo) (ix + 1) res

tiltCycle :: RockMap -> RockMap
tiltCycle = tiltEast . tiltSouth . tiltWest . tiltNorth

tiltSouth :: RockMap -> RockMap
tiltSouth rockMap = foldl' tiltColumnSouth rockMap [0 .. rockMap ^. #width - 1]
  where
    tiltColumnSouth :: RockMap -> Int -> RockMap
    tiltColumnSouth rocks column =
        fst $
            foldl'
                (\(rm, ps) y -> move ps rm (column, y))
                (rocks, Empty)
                [(rocks ^. #height) - 1, (rocks ^. #height) - 2 .. 0]

tiltEast :: RockMap -> RockMap
tiltEast rockMap = foldl' tiltColumnEast rockMap [0 .. rockMap ^. #height - 1]
  where
    tiltColumnEast :: RockMap -> Int -> RockMap
    tiltColumnEast rocks row =
        fst $
            foldl'
                (\(rm, ps) x -> move ps rm (x, row))
                (rocks, Empty)
                [rocks ^. #width - 1, rocks ^. #width - 2 .. 0]

tiltWest :: RockMap -> RockMap
tiltWest rockMap = foldl' tiltColumnWest rockMap [0 .. rockMap ^. #height - 1]
  where
    tiltColumnWest :: RockMap -> Int -> RockMap
    tiltColumnWest rocks row =
        fst $
            foldl'
                (\(rm, ps) x -> move ps rm (x, row))
                (rocks, Empty)
                [0, 1 .. rocks ^. #width - 1]
