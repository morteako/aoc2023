module CmdArgs where

import qualified Data.Bifunctor as Bifunctor
import DayVersion
import Options.Applicative as Opt
import Text.Megaparsec as Parsec
import Text.Read (readMaybe)

data Options = Options
  { day :: Day
  , input :: Input
  , year :: Int
  }
  deriving (Show)

data Day = LastDay | SpecificDay DayVersion deriving (Show)

data Input = StdIn | File String | Test | DayInput deriving (Show)

megaparsecReader :: Parsec String String a -> ReadM a
megaparsecReader parser =
  eitherReader (Bifunctor.first show . Parsec.parse parser "")

cmdParser :: DayVersion -> ParserInfo Options
cmdParser lastDay =
  info
    (options <**> helper)
    ( fullDesc
        <> progDesc ("Run a advent of code challenge. Default is to run the last implemented challenge (" <> show lastDay <> ") and fetch the corresponding input")
        <> header "aoc2021 - haskell solutions for advent of code 2021"
    )

options :: Parser Options
options =
  Options
    <$> (specificDayInput <|> pure LastDay)
    <*> (stdInput <|> fileInput <|> testInput <|> pure DayInput)
    <*> yearInput

yearInput :: Parser Int
yearInput =
  Opt.option
    auto
    ( long "year"
        <> metavar "YEAR"
        <> value 2020
        <> showDefault
        <> help "Chose which year of AoC"
    )

specificDayInput :: Parser Day
specificDayInput =
  SpecificDay
    <$> strOption
      ( long "day"
          <> metavar "DAY"
          <> help "Run challenge for the provided day"
      )

fileInput :: Parser Input
fileInput =
  File
    <$> strOption
      ( long "file"
          <> metavar "FILENAME"
          <> help "Read from input file"
      )

stdInput :: Parser Input
stdInput =
  flag'
    StdIn
    ( long "stdin"
        <> help "Read from stdin"
    )

testInput :: Parser Input
testInput =
  flag'
    Test
    ( short 'T'
        <> long "test"
        <> help "Reads from 'input/DAYtest'"
    )
