module Solutions where

import Data.Map (Map)
import Data.Map qualified as Map
import Day.Day01 qualified as Day01
import Day.Day02 qualified as Day02
import Day.Day03 qualified as Day03
import Day.Day04 qualified as Day04
import Day.Day06 qualified as Day06
import DayVersion
import Utils ((=:))

solutions :: Map DayVersion (String -> IO ())
solutions =
  Map.fromList
    [ NormalDay 1 =: Day01.run
    , NormalDay 2 =: Day02.run
    , NormalDay 3 =: Day03.run
    , NormalDay 4 =: Day04.run
    , NormalDay 6 =: Day06.run
    ]
