module Solutions where

import Data.Map (Map)
import Data.Map qualified as Map
import Day.Day01 qualified as Day01
import DayVersion
import Utils ((=:))

solutions :: Map DayVersion (String -> IO ())
solutions =
  Map.fromList
    [ NormalDay 1 =: Day01.run
    ]
