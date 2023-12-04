{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedLabels #-}

module Solutions.Day04 where

import MyPrelude

import Control.Monad.Reader qualified as Reader
import Data.IntMap.Strict qualified as IM
import Data.IntSet qualified as IS
import Optics ((^.))
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as Lex

-- * Part 1

solve1 :: Text -> Text
solve1 = showt . sum . fmap cardScore . partialParse inputP

cardScore :: Card -> Int
cardScore card =
    let n = winnerCount card
    in if n > 0 then 2 ^ (n - 1) else 0

-- | Return the number of winning numbers for a 'Card'.
winnerCount :: Card -> Int
winnerCount card = IS.size (IS.intersection (card ^. #winningNums) (card ^. #myNums))

data Card = Card {
    id :: Int,
    winningNums :: IntSet,
    myNums :: IntSet
    }
    deriving (Eq, Show, Generic)

-- ** Parser

type Parser = P.Parsec Void Text

cardP :: Parser Card
cardP = do
    cardId <- P.string "Card" *> P.hspace1 *> Lex.decimal <* P.char ':'
    _ <- P.hspace
    winningNumbers <- P.sepEndBy Lex.decimal P.hspace1
    _ <- P.hspace *> P.char '|' <* P.hspace
    myNumbers <- P.sepBy Lex.decimal P.hspace1
    _ <- P.eol

    pure $ Card cardId (IS.fromList winningNumbers) (IS.fromList myNumbers)

inputP :: Parser [Card]
inputP = P.someTill cardP P.eof

-- * Part 2

solve2 :: Text -> Text
solve2 =
    showt
    . fst
    . (\(cardMap :: IntMap Card) ->
        IM.foldl'
            (\(accCount, accMem) card ->
                first (accCount +) (Reader.runReader (calculateCopies card accMem) cardMap))
            (0, mempty)
            cardMap)
    . IM.fromList
    . fmap (\c -> (c ^. #id, c))
    . partialParse inputP

-- | Maps 'Card' index to the number of copies gained from its prize (not
-- including itself).
type CopiesMemory = IntMap Int

calculateCopies :: Card -> CopiesMemory -> Reader (IntMap Card) (Int, CopiesMemory)
calculateCopies card mem = case IM.lookup cardId mem of
    Just n -> pure (1 + n, mem)
    Nothing -> do
        cardMap <- Reader.ask
        let prizeIds :: [Int] = [n | n <- [cardId + 1 .. cardId + winnerCount card], n /= cardId]
        let prizes :: [Card] = fmap (cardMap IM.!) prizeIds
        (numCopies, newMem) <-
            foldM
                (\(accCopies, accMem) c -> first (+ accCopies) <$> calculateCopies c accMem)
                (0, mem)
                prizes
        pure (1 + numCopies, newMem)
  where
    cardId :: Int
    cardId = card ^. #id
