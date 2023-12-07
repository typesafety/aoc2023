{-# language DeriveGeneric #-}
{-# language DuplicateRecordFields #-}
{-# language LambdaCase #-}
{-# language NoFieldSelectors #-}
{-# language NoImplicitPrelude #-}
{-# language OverloadedLabels #-}
{-# language OverloadedLists #-}
{-# language OverloadedStrings #-}
{-# language PackageImports #-}

module Solutions.Day07 where

import MyPrelude

import "base" Control.Arrow ((>>>))
import "base" Data.List qualified as List
import "base" Text.Read (readMaybe)

import "megaparsec" Text.Megaparsec qualified as P
import "megaparsec" Text.Megaparsec.Char qualified as P
import "megaparsec" Text.Megaparsec.Char.Lexer qualified as Lex
import "optics" Optics ((^.), view)

-- * Part 1

solve1 :: Text -> Text
solve1 = showt
    . sum
    . List.zipWith (\ix camelHand -> ix * (camelHand ^. #bid)) [1 ..]
    . List.sortOn (Hand . view #cards)
    . partialParse inputP

handType :: [Int] -> HandType
handType = counts >>> \case
    (1, 5) -> HighCard
    (2, 4) -> OnePair
    (2, 3) -> TwoPairs
    (3, 3) -> ThreeOfAKind
    (3, 2) -> FullHouse
    (4, 2) -> FourOfAKind
    (5, 1) -> FiveOfAKind
    _ -> error "boo"
  where
    counts :: [Int] -> (Int, Int)
    counts = go 0 0 0 (-1) . List.sort
      where
        -- types document themselves lol
        go :: Int -> Int -> Int -> Int -> [Int] -> (Int, Int)
        go top groups count _ [] = (max top count, groups)
        go top groups count prev (n : ns) = if prev == n
            then go top groups (count + 1) n ns
            else go (max top count) (groups + 1) 1 n ns

data HandType
    = HighCard
    | OnePair
    | TwoPairs
    | ThreeOfAKind
    | FullHouse
    | FourOfAKind
    | FiveOfAKind
    deriving (Eq, Ord, Show)
    -- NOTE: Deriving Ord makes the order in which the HandType constructors are
    -- declared in matter ("lowest" is declared first).

newtype Hand = Hand { cards :: [Int] }
    deriving (Eq, Show, Generic)

instance Ord Hand where
    compare :: Hand -> Hand -> Ordering
    compare (Hand h1) (Hand h2) = case compare (handType h1) (handType h2) of
        EQ -> secondaryCompare h1 h2
        ordering -> ordering
      where
        secondaryCompare :: [Int] -> [Int] -> Ordering
        secondaryCompare xs ys =
            fromMaybe EQ . find (/= EQ) $ List.zipWith compare xs ys

data CamelHand = CamelHand {
    cards :: [Int],
    bid :: Int
    }
    deriving (Eq, Show, Generic)

-- ** Parsing

type Parser = P.Parsec Void Text

inputP :: Parser [CamelHand]
inputP = P.someTill handP P.eof

handP :: Parser CamelHand
handP = do
    cards <- fmap (fmap cardValue) (P.count 5 (P.upperChar <|> P.digitChar))
    _ <- P.hspace1
    bid <- Lex.decimal
    _ <- P.eol
    pure $ CamelHand cards bid
  where
    cardValue :: Char -> Int
    cardValue = \case
        'T' -> 10
        'J' -> 11
        'Q' -> 12
        'K' -> 13
        'A' -> 14
        c -> fromMaybe (error $ "Couldn't parse char: " <> [c]) (readMaybe [c])

-- * Part 2

solve2 :: Text -> Text
solve2 = showt
    . sum
    . List.zipWith (\ix camelHand -> ix * (camelHand ^. #bid)) [1 ..]
    . List.sortOn (JokerRules . view #cards)
    . partialParse inputP

applyStrongJoker :: [Int] -> [Int]
applyStrongJoker cards = case List.filter (/= 11) cards of
    [] -> cards
    xs ->
        let target = partialLast . concat . List.sortOn List.length . List.group . List.sort $ xs
        in fmap (\n -> if n == 11 then target else n) cards

applyWeakJoker :: [Int] -> [Int]
applyWeakJoker = fmap (\n -> if n == 11 then 1 else n)

newtype JokerRules = JokerRules { cards :: [Int] }
    deriving (Eq, Show, Generic)

instance Ord JokerRules where
    compare :: JokerRules -> JokerRules -> Ordering
    compare (JokerRules h1) (JokerRules h2) =
        case (uncurry compare . both (handType . applyStrongJoker)) (h1, h2) of
            EQ -> secondaryCompare h1 h2
            ordering -> ordering
      where
        secondaryCompare :: [Int] -> [Int] -> Ordering
        secondaryCompare xs ys =
            fromMaybe EQ
            . find (/= EQ)
            . uncurry (List.zipWith compare)
            . both applyWeakJoker
            $ (xs, ys)
