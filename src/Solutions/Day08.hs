{-# language DeriveGeneric #-}
{-# language DuplicateRecordFields #-}
{-# language NoFieldSelectors #-}
{-# language NoImplicitPrelude #-}
{-# language OverloadedLabels #-}
{-# language OverloadedLists #-}
{-# language OverloadedStrings #-}
{-# language PackageImports #-}

module Solutions.Day08 where

import MyPrelude

import "base" Data.List qualified as List

import "megaparsec" Text.Megaparsec qualified as P
import "megaparsec" Text.Megaparsec.Char qualified as P
import "optics" Optics
import "unordered-containers" Data.HashMap.Strict qualified as HM

-- * Part 1

solve1 :: Text -> Text
solve1 = showt
    . List.length
    . (\(instrs, nodes) -> path (nodeMap nodes) (List.cycle instrs))
    . partialParse inputP

path :: NodeMap -> [Instruction] -> [String]
path = go "AAA"
  where
    go :: String -> NodeMap -> [Instruction] -> [String]
    go current m instrs = case current of 
        "ZZZ" -> []
        _ -> case instrs of
            (i : is) ->
                let next = step m current i
                in next : go next m is
            [] -> []

step :: NodeMap -> String -> Instruction -> String
step m name instr =
    let (leftDest, rightDest) = m HM.! name
    in case instr of
        L -> leftDest
        R -> rightDest

nodeMap :: [Node] -> NodeMap
nodeMap = HM.fromList . fmap (\node -> (node ^. #name, (node ^. #left, node ^. #right)))

type NodeMap = HashMap String (String, String)

data Instruction = L | R
    deriving (Eq, Show)

data Node = Node {
    name :: String,
    left :: String,
    right :: String
    }
    deriving (Eq, Show, Generic)

-- ** Parsing

type Parser = P.Parsec Void Text

inputP :: Parser ([Instruction], [Node])
inputP = do
    instructions <- instructionsP
    _ <- P.eol
    nodes <- P.sepEndBy1 nodeP P.eol
    _ <- P.eof
    pure (instructions, nodes)

instructionsP :: Parser [Instruction]
instructionsP = P.someTill (P.char 'L' $> L <|> P.char 'R' $> R) P.eol

nodeP :: Parser Node
nodeP = do
    name <- nodeNameP
    left <- P.string " = (" *> nodeNameP
    right <- P.string ", " *> nodeNameP <* ")"
    pure (Node name left right)
  where
    nodeNameP :: Parser String
    nodeNameP = P.count 3 P.alphaNumChar


-- * Part 2

solve2 :: Text -> Text
solve2 = todo
