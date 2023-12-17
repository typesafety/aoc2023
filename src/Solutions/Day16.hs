{-# language DuplicateRecordFields #-}
{-# language LambdaCase #-}
{-# language NoFieldSelectors #-}
{-# language NoImplicitPrelude #-}
{-# language PackageImports #-}

module Solutions.Day16 where

import MyPrelude

import "base" Data.List qualified as List

import "hashable" Data.Hashable (Hashable)
import "megaparsec" Text.Megaparsec qualified as P
import "megaparsec" Text.Megaparsec.Char qualified as P
import "mtl" Control.Monad.Reader qualified as Reader
import "mtl" Control.Monad.State.Strict qualified as State
import "unordered-containers" Data.HashMap.Strict qualified as HM
import "unordered-containers" Data.HashSet qualified as HS

-- * Part 1

solve1 :: Text -> Text
solve1 =
    showt
    . HM.size
    . HM.filter (> 0)
    . runWalk (R, (0, 0))
    . toGrid
    . partialParse inputP

runWalk :: (Direction, Point) -> Grid -> EnergyMap
runWalk loc grid = State.evalState (Reader.runReaderT (walk loc emptyEnergy) grid) mempty
  where
    emptyEnergy :: EnergyMap
    emptyEnergy = HM.fromList . fmap (, 0) . HM.keys $ grid

walk ::
    (Direction, Point) ->
    EnergyMap ->
    ReaderT Grid (State (HashSet (Direction, Point))) EnergyMap
walk (dir, current) em = State.gets (HS.member (dir, current)) >>= \case
    True -> pure em
    False -> do
        let newEm = HM.insert current 1 em
        State.modify' (HS.insert (dir, current))

        Reader.asks (HM.lookup current) >>= \case
            -- Outside grid
            Nothing -> pure em

            Just MirrorSlash -> case dir of
                U -> walk (R, first succ current) newEm
                D -> walk (L, first pred current) newEm
                R -> walk (U, second pred current) newEm
                L -> walk (D, second succ current) newEm

            Just MirrorBackslash -> case dir of
                U -> walk (L, first pred current) newEm
                D -> walk (R, first succ current) newEm
                R -> walk (D, second succ current) newEm
                L -> walk (U, second pred current) newEm

            Just SplitterV
                | dir `elem` [L, R] ->
                    HM.unionWith (+)
                        <$> walk (U, second pred current) newEm
                        <*> walk (D, second succ current) newEm

            Just SplitterH
                | dir `elem` [U, D] ->
                    HM.unionWith (+)
                        <$> walk (L, first pred current) newEm
                        <*> walk (R, first succ current) newEm

            -- Empty or pointy end of splitter; continue in the same direction
            Just _ -> walk (dir, forward dir current) newEm
  where
    forward :: Direction -> Point -> Point
    forward d (x_, y_) = case d of
        U -> (x_, y_ - 1)
        D -> (x_, y_ + 1)
        L -> (x_ - 1, y_)
        R -> (x_ + 1, y_)

toGrid :: [[Tile]] -> Grid
toGrid = foldl' (<>) mempty . List.zipWith go [0 ..]
  where
    go :: Int -> [Tile] -> Grid
    go y = HM.fromList . List.zipWith (\x s -> ((x, y), s)) [0 ..]

data Direction
    = U
    | D
    | L
    | R
    deriving (Eq, Show, Generic)

instance Hashable Direction

type EnergyMap = HashMap Point Int  -- Value > 0 => Energized
type Grid = HashMap Point Tile
type Point = (Int, Int)

data Tile
    = None
    | MirrorSlash
    | MirrorBackslash
    | SplitterV
    | SplitterH
    deriving (Eq, Show)

-- ** Parsing

type Parser = P.Parsec Void Text

inputP :: Parser [[Tile]]
inputP = P.someTill rowP P.eof

rowP :: Parser [Tile]
rowP = P.someTill tileP P.eol

tileP :: Parser Tile
tileP = P.char '.' $> None
    <|> P.char '/' $> MirrorSlash
    <|> P.char '\\' $> MirrorBackslash
    <|> P.char '|' $> SplitterV
    <|> P.char '-' $> SplitterH

-- * Part 2

solve2 :: Text -> Text
solve2 = todo
