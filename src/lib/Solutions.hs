module Solutions where

import Data.Map (Map)
import Data.Map qualified as Map
import Day.Day09 qualified
import Day.Day10 qualified
import Day.Day11 qualified
import DayVersion (DayVersion (NormalDay, SpecialVersion))
import Utils ((=:))

solutions :: Map DayVersion (String -> IO ())
solutions =
  Map.fromList
    [ "01" =: Day.Day01.run
    , "02" =: Day.Day02.run
    , "03" =: Day.Day03.run
    , "03Monoid" =: Day.Day03Monoid.run
    , "04" =: Day.Day04.run
    , "05" =: Day.Day05.run
    , "05StateLens" =: Day.Day05StateLens.run
    , "06" =: Day.Day06.run
    , "07" =: Day.Day07.run
    , "08" =: Day.Day08.run
    , "10" =: Day.Day10.run
    , "11" =: Day.Day11.run
    , "09" =: Day.Day09.run
    , "12" =: Day.Day12.run
    , "13" =: Day.Day13.run
    , "14" =: Day.Day14.run
    , "20" =: Day.Day20.run
    , "21" =: Day.Day21.run
    , "23" =: Day.Day23.run
    , "25" =: Day.Day25.run
    ]
    []
