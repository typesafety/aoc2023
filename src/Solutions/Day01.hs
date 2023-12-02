module Solutions.Day01 where

import MyPrelude

import Control.Arrow ((&&&))
import Data.Char (isDigit)
import Text.Read (readMaybe)

solve1 :: Text -> Text
solve1 = showt . sum . fmap (calibrateValue . partialNonEmpty) . lines . unpackt

calibrateValue :: NonEmpty Char -> Int
calibrateValue chars =
    let (d1, d2) = (partialHead &&& partialLast) (filter isDigit chars)
    in partialFromJust $ readMaybe @Int [d1, d2]

solve2 :: Text -> Text
solve2 = todo

