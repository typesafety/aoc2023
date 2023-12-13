{-# language DuplicateRecordFields #-}
{-# language NoFieldSelectors #-}
{-# language NoImplicitPrelude #-}
{-# language OverloadedLists #-}
{-# language PackageImports #-}

module Solutions.Day12 where

import MyPrelude

import "base" Data.List qualified as List

import "megaparsec" Text.Megaparsec qualified as P
import "megaparsec" Text.Megaparsec.Char qualified as P
import "megaparsec" Text.Megaparsec.Char.Lexer qualified as Lex

-- * Part 1

solve1 :: Text -> Text
solve1 = showt
    . sum
    . fmap (List.length . uncurry variants)
    . partialParse inputP

variants :: [Maybe Spring] -> [Int] -> [Seq Spring]
variants springs = go False (springs, [])
  where
    go :: Bool -> ([Maybe Spring], Seq Spring) -> [Int] -> [Seq Spring]

    go _ (x : xs, acc) [] = case x of
        Just Bad -> []
        Just Good -> go False (xs, acc |> Good) []
        Nothing -> go False (xs, acc |> Good) []

    go _ ([], acc) ns = case ns of
        [] -> [acc]
        [0] -> [acc]
        _ -> []

    go _ (x : xs, acc) (0 : ns) = case x of
        Just Bad -> []
        Just Good -> go False (xs, acc |> Good) ns
        Nothing -> go False (xs, acc |> Good) ns

    go inGroup (x : xs, acc) (badsLeft : bs) = case x of
        Just Good
            | inGroup -> []
            | otherwise -> go False (xs, acc |> Good) (badsLeft : bs)
        Just Bad -> go True (xs, acc |> Bad) (pred badsLeft : bs)
        Nothing
            | inGroup -> go True (xs, acc |> Bad) (pred badsLeft : bs)
            | otherwise ->
                let v1 = go True (xs, acc |> Bad) (pred badsLeft : bs)
                    v2 = go False (xs, acc |> Good) (badsLeft : bs)
                in v1 <> v2

data Spring = Good | Bad
    deriving (Eq, Show)

-- ** Parsing

type Parser = P.Parsec Void Text

inputP :: Parser  [([Maybe Spring], [Int])]
inputP = P.sepEndBy1 lineP P.eol <* P.eof
  where
    lineP :: Parser ([Maybe Spring], [Int])
    lineP = do
        springs <- P.someTill springP P.hspace1
        groups <- P.sepBy1 Lex.decimal (P.char ',')
        pure (springs, groups)
      where
        springP :: Parser (Maybe Spring)
        springP = P.char '.' $> Just Good
            <|> P.char '#' $> Just Bad
            <|> P.char '?' $> Nothing

-- * Part 2

solve2 :: Text -> Text
solve2 = todo
