{-# language DuplicateRecordFields #-}
{-# language NoFieldSelectors #-}
{-# language NoImplicitPrelude #-}
{-# language OverloadedLabels #-}
{-# language PackageImports #-}

module Solutions.Day15 where

import MyPrelude

import "base" Data.Char (isAscii, ord)

import "containers" Data.IntMap.Strict qualified as IM
import "containers" Data.Sequence qualified as Seq
import "megaparsec" Text.Megaparsec qualified as P
import "megaparsec" Text.Megaparsec.Char qualified as P
import "megaparsec" Text.Megaparsec.Char.Lexer qualified as Lex
import "optics" Optics ((^.))

-- * Part 1

solve1 :: Text -> Text
solve1 = showt . foldl' (\acc step -> acc + hash step) 0 . partialParse inputP

hash :: String -> Int
hash = foldl' hashChar 0
  where
    hashChar :: Int -> Char -> Int
    hashChar current = (`rem` 256) . (* 17) . (+ current) . ord

-- ** Parsing

type Parser = P.Parsec Void Text

inputP :: Parser [String]
inputP = P.sepBy stepP (P.char ',') <* P.eol
  where
    stepP :: Parser String
    stepP = P.some (P.satisfy (\c -> isAscii c && c `notElem` ",\n"))

-- * Part 2

solve2 :: Text -> Text
solve2 = showt . focusingPower . performSteps . partialParse inputP2

focusingPower :: Boxes -> Int
focusingPower =
    IM.foldlWithKey'
        (\acc boxNo lenses ->
            acc + Seq.foldlWithIndex
                (\acc2 ix lens -> acc2 + (1 + boxNo) * (ix + 1) * (lens ^. #focal))
                0
                lenses
            )
        0

performSteps :: [Step] -> Boxes
performSteps = foldl' performStep (IM.fromList (fmap (, Seq.empty) [0 .. 255]))

performStep :: Boxes -> Step -> Boxes
performStep boxes step =
    let label = step ^. #label
        boxNo = hashLabel label
    in case step ^. #op of
        Dash -> IM.adjust (removeLens label) boxNo boxes
        Equals focal -> IM.adjust (insertLens (Lens label focal)) boxNo boxes
  where
    insertLens :: Lens -> Seq Lens -> Seq Lens
    insertLens lens Empty = Seq.singleton lens
    insertLens lens (l :<| ls)
        | lens ^. #label == l ^. #label = lens <| ls
        | otherwise = l <| insertLens lens ls

    removeLens :: Label -> Seq Lens -> Seq Lens
    removeLens _ Empty = Empty
    removeLens label (l :<| ls)
        | label == l ^. #label = ls
        | otherwise = l <| removeLens label ls

    hashLabel :: Label -> Int
    hashLabel (Label s) = hash s

type Boxes = IntMap (Seq Lens)

data Lens = Lens {
    label :: Label,
    focal :: Int
    }
    deriving (Eq, Show, Generic)

data Step = Step {
    label :: Label,
    op :: Operation
    }
    deriving (Eq, Show, Generic)

newtype Label = Label String
    deriving (Eq, Show)

data Operation
    = Dash
    | Equals Int
    deriving (Eq, Show)

-- ** Parsing

inputP2 :: Parser [Step]
inputP2 = P.sepBy stepP2 (P.char ',') <* P.eol
  where
    stepP2 :: Parser Step
    stepP2 = Step . Label <$> P.some P.lowerChar <*> opP

    opP :: Parser Operation
    opP = P.char '-' $> Dash
        <|> Equals <$> (P.char '=' *> Lex.decimal)
