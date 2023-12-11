module Solutions where

import Data.Map (Map)
import Data.Map qualified as Map
import Day.Day01 qualified as Day01
import Day.Day02 qualified as Day02
import Day.Day03 qualified as Day03
import Day.Day04 qualified as Day04
import Day.Day05 qualified as Day05
import Day.Day06 qualified as Day06
import Day.Day07 qualified as Day07
import Day.Day08 qualified as Day08
import Day.Day09 qualified as Day09
import Day.Day10 qualified as Day10
import DayVersion
import Utils ((=:))

solutions :: Map DayVersion (String -> IO ())
solutions =
  Map.fromList
    [ "1" =: Day01.run
    , "2" =: Day02.run
    , "3" =: Day03.run
    , "4" =: Day04.run
    , "5" =: Day05.run
    , "6" =: Day06.run
    , "7" =: Day07.run
    , "8" =: Day08.run
    , "9" =: Day09.run
    , "10" =: Day10.run
    ]
