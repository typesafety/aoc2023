{-# language DuplicateRecordFields #-}
{-# language NoFieldSelectors #-}
{-# language NoImplicitPrelude #-}
{-# language PackageImports #-}

module Solutions.Day13 where

import MyPrelude

import "base" Data.List qualified as List

import "containers" Data.Sequence qualified as Sequence
import "megaparsec" Text.Megaparsec qualified as P
import "megaparsec" Text.Megaparsec.Char qualified as P

-- * Part 1

solve1 :: Text -> Text
solve1 = showt . sum . fmap (partialFromJust . patternNote) . partialParse inputP

patternNote :: [[Crap]] -> Maybe Int
patternNote crap = horizNote crap <|> vertNote crap
  where
    vertNote :: [[Crap]] -> Maybe Int
    vertNote =
        fmap (length . fst)
        . findMiddle Empty
        . Sequence.fromList
        . List.transpose

    horizNote :: [[Crap]] -> Maybe Int
    horizNote =
        fmap ((* 100) . length . fst)
        . findMiddle Empty
        . Sequence.fromList

findMiddle :: Seq [Crap] -> Seq [Crap] -> Maybe (Seq [Crap], Seq [Crap])
findMiddle before after = case (before, after) of
    (_ :|> latest, next :<| rest)
        | latest == next
            && isMiddle (Sequence.reverse before) after -> Just (before, after)
        | otherwise -> findMiddle (before |> next) rest
    (_, Empty) -> Nothing
    (Empty, next :<| rest) -> findMiddle (Empty |> next) rest
  where
    isMiddle :: Seq [Crap] -> Seq [Crap] -> Bool
    isMiddle x y = and (Sequence.zipWith (==) x y)

data Crap = Ash | Rock
    deriving (Eq, Show)

-- ** Parsing

type Parser = P.Parsec Void Text

inputP :: Parser [[[Crap]]]
inputP = P.sepBy1 patternP P.eol
  where
    patternP :: Parser [[Crap]]
    patternP = P.some rowP

    rowP :: Parser [Crap]
    rowP = P.someTill crapP P.eol

    crapP :: Parser Crap
    crapP = P.char '.' $> Ash <|> P.char '#' $> Rock

-- * Part 2

solve2 :: Text -> Text
solve2 = showt
    . sum
    . fmap (
        partialFromJust
        . uncurry patternNote2
        . partialHead
        . List.filter (\(variant, orig) ->
            let split =
                    fmap
                        ((, 'h') . length . fst)
                        (findMiddles Empty (Sequence.fromList variant))
                    <>
                    fmap
                        ((, 'v') . length . fst)
                        (findMiddles Empty (Sequence.fromList (List.transpose variant)))
                origSplit =
                    fmap
                        ((, 'h') . length . fst)
                        (findMiddles Empty (Sequence.fromList orig))
                    <>
                    fmap
                        ((, 'v') . length . fst)
                        (findMiddles Empty (Sequence.fromList (List.transpose orig)))
            in (split /= origSplit) && split /= []
            )
        . (\pat -> fmap (, pat) (patternVariants pat))
        )
    . partialParse inputP

findMiddles :: Seq [Crap] -> Seq [Crap] -> [(Seq [Crap], Seq [Crap])]
findMiddles before after = case (before, after) of
    (_ :|> latest, next :<| rest)
        | latest == next
            && isMiddle (Sequence.reverse before) after ->
                (before, after) : findMiddles (before |> next) rest
        | otherwise -> findMiddles (before |> next) rest
    (_, Empty) -> []
    (Empty, next :<| rest) -> findMiddles (Empty |> next) rest
  where
    isMiddle :: Seq [Crap] -> Seq [Crap] -> Bool
    isMiddle x y = and (Sequence.zipWith (==) x y)

patternNote2 :: [[Crap]] -> [[Crap]] -> Maybe Int
patternNote2 crap orig =
    let scores = horizNote crap <> vertNote crap
    in if null scores
        then Nothing
        else Just (partialHead (List.filter (\score -> Just score /= patternNote orig) scores))
  where
    vertNote :: [[Crap]] -> [Int]
    vertNote =
        fmap (length . fst)
        . List.filter (\p -> Just p /= findMiddle Empty (Sequence.fromList . List.transpose $ orig))
        . findMiddles Empty
        . Sequence.fromList
        . List.transpose

    horizNote :: [[Crap]] -> [] Int
    horizNote =
        fmap ((* 100) . length . fst)
        . List.filter (\p -> Just p /= findMiddle Empty (Sequence.fromList orig))
        . findMiddles Empty
        . Sequence.fromList

rowVariants :: [Crap] -> [[Crap]]
rowVariants row = List.zipWith go [0 ..] $ replicate (length row) row
  where
    go :: Int -> [Crap] -> [Crap]
    go _ [] = error "poo"
    go 0 (x : xs) = smudge x : xs
    go n (x : xs) = x : go (n - 1) xs

    smudge :: Crap -> Crap
    smudge Ash = Rock
    smudge Rock = Ash

patternVariants :: [[Crap]] -> [[[Crap]]]
patternVariants rows =
    mconcat $ List.zipWith (`go` []) [0 ..] (replicate (length rows) rows)
  where
    go :: Int -> [[Crap]] -> [[Crap]] -> [[[Crap]]]
    go _ _ [] = error "poo2"
    go 0 rowsBefore (target : rowsAfter) = fmap (\row -> rowsBefore <> [row] <> rowsAfter) (rowVariants target)
    go n ys (x : xs) = go (n - 1) (ys <> [x]) xs
