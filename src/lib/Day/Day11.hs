module Day.Day11 (run) where

import Control.Arrow ((>>>))
import Control.Monad (guard)
import Data.List (transpose)
import Test.HUnit ((@=?))

parse :: String -> Integer -> [(Integer, Integer)]
parse str (pred -> repl) = lines >>> addGalaxies >>> filter ((== '#') . snd) >>> map fst $ str
 where
  addGalaxies ls = zip ((,) <$> indexes ls <*> indexes (transpose ls)) (concat ls)

  indexes = scanl1 (+) . map (\line -> if all (== '.') line then repl + 1 else 1)

mandist (x, y) (xx, yy) = abs (x - xx) + abs (y - yy)

sumDistances :: [(Integer, Integer)] -> Integer
sumDistances galaxies =
  sum $ do
    k <- galaxies
    k2 <- galaxies
    guard (k < k2)
    pure $ mandist k k2

run :: String -> IO ()
run (parse -> parsed) = do
  let resA = sumDistances (parsed 2)
  print resA
  let resB = sumDistances (parsed 1000000)
  print resB
  resB @=? 512240933238