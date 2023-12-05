{-# language LambdaCase #-}
{-# language NoImplicitPrelude #-}
{-# language OverloadedLists #-}
{-# language OverloadedStrings #-}
{-# language PackageImports #-}

module Main where

import MyPrelude

import "filepath" System.FilePath ((</>))
import "optparse-applicative" Options.Applicative (
    (<**>),
    Parser,
    ParserInfo,
    execParser,
    fullDesc,
    header,
    help,
    helper,
    info,
    long,
    maybeReader,
    option,
    optional,
    progDesc,
    strOption,
    )

import Solutions.Day01 qualified as S01
import Solutions.Day02 qualified as S02
import Solutions.Day03 qualified as S03
import Solutions.Day04 qualified as S04
import Solutions.Day05 qualified as S05


main :: IO ()
main = do
    args <- parseArgs

    putTextLn $ "=== " <> showt' (args_day args) <> ", " <> showt' (args_part args) <> " ==="

    inputText <- case args_inputFileOverride args of
        Just fp -> readPuzzleInputOverride fp
        Nothing -> readPuzzleInput (args_day args) (args_inputsDir args)
    let solver = pickSolver (args_day args) (args_part args)

    putTextLn $ solver inputText

-- * Input file reading

readPuzzleInput :: Day -> FilePath -> IO Text
readPuzzleInput day inputDir = do
    let inputFp = inputDir </> (show (dayToInt day) <> ".txt")
    packt <$> readFile inputFp

readPuzzleInputOverride :: FilePath -> IO Text
readPuzzleInputOverride = fmap packt . readFile

-- * Solver selection

pickSolver :: Day -> Part -> (Text -> Text)
pickSolver d p = case (d, p) of
    (D01, P1) -> S01.solve1
    (D01, P2) -> S01.solve2
    (D02, P1) -> S02.solve1
    (D02, P2) -> S02.solve2
    (D03, P1) -> S03.solve1
    (D03, P2) -> S03.solve2
    (D04, P1) -> S04.solve1
    (D04, P2) -> S04.solve2
    (D05, P1) -> S05.solve1
    (D05, P2) -> S05.solve2
    -- (D06, P1) -> S06.solve1
    -- (D06, P2) -> S06.solve2
    -- (D07, P1) -> S07.solve1
    -- (D07, P2) -> S07.solve2
    -- (D08, P1) -> S08.solve1
    -- (D08, P2) -> S08.solve2
    -- (D09, P1) -> S09.solve1
    -- (D09, P2) -> S09.solve2
    -- (D10, P1) -> S10.solve1
    -- (D10, P2) -> S10.solve2
    -- (D11, P1) -> S11.solve1
    -- (D11, P2) -> S11.solve2
    -- (D12, P1) -> S12.solve1
    -- (D12, P2) -> S12.solve2
    -- (D13, P1) -> S13.solve1
    -- (D13, P2) -> S13.solve2
    -- (D14, P1) -> S14.solve1
    -- (D14, P2) -> S14.solve2
    -- (D15, P1) -> S15.solve1
    -- (D15, P2) -> S15.solve2
    -- (D16, P1) -> S16.solve1
    -- (D16, P2) -> S16.solve2
    -- (D17, P1) -> S17.solve1
    -- (D17, P2) -> S17.solve2
    -- (D18, P1) -> S18.solve1
    -- (D18, P2) -> S18.solve2
    -- (D19, P1) -> S19.solve1
    -- (D19, P2) -> S19.solve2
    -- (D20, P1) -> S20.solve1
    -- (D20, P2) -> S20.solve2
    -- (D21, P1) -> S21.solve1
    -- (D21, P2) -> S21.solve2
    -- (D22, P1) -> S22.solve1
    -- (D22, P2) -> S22.solve2
    -- (D23, P1) -> S23.solve1
    -- (D23, P2) -> S23.solve2
    -- (D24, P1) -> S24.solve1
    -- (D24, P2) -> S24.solve2
    -- (D25, P1) -> S25.solve1
    -- (D25, P2) -> S25.solve2
    _ -> error $ "Solution not implemented for " <> show d <> ", " <> show p

-- * Argument parsing

data Part = P1 | P2
    deriving (Eq, Show)

data Day
    = D01
    | D02
    | D03
    | D04
    | D05
    | D06
    | D07
    | D08
    | D09
    | D10
    | D11
    | D12
    | D13
    | D14
    | D15
    | D16
    | D17
    | D18
    | D19
    | D20
    | D21
    | D22
    | D23
    | D24
    | D25
    deriving (Eq, Show)

dayToInt :: Day -> Int
dayToInt = \case
    D01 -> 1
    D02 -> 2
    D03 -> 3
    D04 -> 4
    D05 -> 5
    D06 -> 6
    D07 -> 7
    D08 -> 8
    D09 -> 9
    D10 -> 10
    D11 -> 11
    D12 -> 12
    D13 -> 13
    D14 -> 14
    D15 -> 15
    D16 -> 16
    D17 -> 17
    D18 -> 18
    D19 -> 19
    D20 -> 20
    D21 -> 21
    D22 -> 22
    D23 -> 23
    D24 -> 24
    D25 -> 25

data Args = Args {
    args_day :: Day,
    args_part :: Part,
    args_inputsDir :: FilePath,
    args_inputFileOverride :: Maybe FilePath
}
    deriving (Eq, Show)

parseArgs :: IO Args
parseArgs = execParser parserInfo
  where
    parserInfo :: ParserInfo Args
    parserInfo =
        info
            (argsParser <**> helper)
            (fullDesc
                <> progDesc "Run the solver for a given day and part."
                <> header "Advent of Code 2023"
            )

argsParser :: Parser Args
argsParser = Args
    <$> dayParser
    <*> partParser
    <*> inputsDirParser
    <*> inputFileOverrideParser
  where
    -- For specifying a specific input file
    inputFileOverrideParser :: Parser (Maybe FilePath)
    inputFileOverrideParser = optional $ strOption
        (long "input_file_override"
            <> help "Specific input file to use. (TODO: make --inputs_directory _xor_ --input_file_override required)")

    inputsDirParser :: Parser FilePath
    inputsDirParser = strOption
        (long "inputs_directory"
            <> help "Directory containing puzzle input files, named '<DAY>.txt', without leading zeroes.")

    dayParser :: Parser Day
    dayParser = option
        (maybeReader dayFromString)
        (long "day")
      where
        dayFromString :: String -> Maybe Day
        dayFromString = \case
            "1" -> Just D01
            "2" -> Just D02
            "3" -> Just D03
            "4" -> Just D04
            "5" -> Just D05
            "6" -> Just D06
            "7" -> Just D07
            "8" -> Just D08
            "9" -> Just D09
            "10" -> Just D10
            "11" -> Just D11
            "12" -> Just D12
            "13" -> Just D13
            "14" -> Just D14
            "15" -> Just D15
            "16" -> Just D16
            "17" -> Just D17
            "18" -> Just D18
            "19" -> Just D19
            "20" -> Just D20
            "21" -> Just D21
            "22" -> Just D22
            "23" -> Just D23
            "24" -> Just D24
            "25" -> Just D25
            _ -> Nothing

    partParser :: Parser Part
    partParser = option
        (maybeReader partFromString)
        (long "part")
      where
        partFromString :: String -> Maybe Part
        partFromString = \case
            "1" -> Just P1
            "2" -> Just P2
            _ -> Nothing
