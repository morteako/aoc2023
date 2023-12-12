module Solutions where

import Data.Map (Map)
import Data.Map qualified as Map
import Day.Day01 qualified
import Day.Day02 qualified
import Day.Day03 qualified
import Day.Day04 qualified
import Day.Day05 qualified
import Day.Day06 qualified
import Day.Day07 qualified
import Day.Day08 qualified
import Day.Day09 qualified
import Day.Day10 qualified
import qualified Day.Day12

import Day.Day11 qualified
import DayVersion
import Utils ((=:))

solutions :: Map DayVersion (String -> IO ())
solutions =
  Map.fromList
    [ "1" =: Day.Day01.run
    , "2" =: Day.Day02.run
    , "3" =: Day.Day03.run
    , "4" =: Day.Day04.run
    , "5" =: Day.Day05.run
    , "6" =: Day.Day06.run
    , "7" =: Day.Day07.run
    , "8" =: Day.Day08.run
    , "9" =: Day.Day09.run
    , "10" =: Day.Day10.run
    , "11" =: Day.Day11.run
    , "12" =: Day.Day12.run
    ]
