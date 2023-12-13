{-# language DuplicateRecordFields #-}
{-# language LambdaCase #-}
{-# language NoFieldSelectors #-}
{-# language NoImplicitPrelude #-}
{-# language OverloadedLists #-}
{-# language PackageImports #-}

module Solutions.Day12 where

import MyPrelude

import "base" Data.List qualified as List

import "hashable" Data.Hashable (Hashable)
import "megaparsec" Text.Megaparsec qualified as P
import "megaparsec" Text.Megaparsec.Char qualified as P
import "megaparsec" Text.Megaparsec.Char.Lexer qualified as Lex
import "mtl" Control.Monad.State.Strict qualified as State
import "unordered-containers" Data.HashMap.Strict qualified as HM

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
    deriving (Eq, Show, Generic)

instance Hashable Spring

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
solve2 = showt
    . sum
    . fmap (uncurry variants2 . unfoldSprings)
    . partialParse inputP

type Memo = HashMap (Bool, [Maybe Spring], [Int]) Int

unfoldSprings :: ([Maybe Spring], [Int]) -> ([Maybe Spring], [Int])
unfoldSprings = bimap (List.intercalate [Nothing] . List.replicate 5) (mconcat . List.replicate 5)

variants2 :: [Maybe Spring] -> [Int] -> Int
variants2 apa bepa = State.evalState (go False apa bepa) HM.empty
  where
    go :: Bool -> [Maybe Spring] -> [Int] -> State Memo Int
    go b xs ns = State.get <&> HM.lookup (b, xs, ns) >>= \case
        Nothing -> go2 b xs ns
        Just n -> pure n

    go2 :: Bool -> [Maybe Spring] -> [Int] -> State Memo Int

    go2 inGroup springs@(x : xs) [] = case x of
        Just Bad -> do
            memoize inGroup springs [] 0
            pure 0
        Just Good -> go False xs []
        Nothing -> go False xs []

    go2 inGroup springs@(x : xs) groups@(0 : ns) = case x of
        Just Bad -> memoize inGroup springs groups 0 $> 0
        Just Good -> go False xs ns
        Nothing -> go False xs ns

    go2 inGroup [] ns = case ns of
        [] -> memoize inGroup [] [] 1 $> 1
        [0] -> memoize inGroup [] [0] 1 $> 1
        _ -> memoize inGroup [] ns 0 $> 0

    go2 inGroup springs@(x : xs) groups@(badsLeft : bs) = case x of
        Just Good
            | inGroup -> memoize inGroup springs groups 0 $> 0
            | otherwise -> go False xs (badsLeft : bs)
        Just Bad -> go True xs (pred badsLeft : bs)
        Nothing
            | inGroup -> go True xs (pred badsLeft : bs)
            | otherwise -> do
                v1 <- go True xs (pred badsLeft : bs)
                v2 <- go False xs (badsLeft : bs)
                memoize inGroup springs groups (v1 + v2)
                pure (v1 + v2)

    memoize :: Bool -> [Maybe Spring] -> [Int] -> Int -> State Memo ()
    memoize b xs ns n = State.modify' (HM.insert (b, xs, ns) n)
