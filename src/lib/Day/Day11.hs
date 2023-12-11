module Day.Day11 (run) where

import Control.Arrow ((>>>))
import Control.Monad (guard)
import Data.List (transpose)
import Data.Map qualified as Map
import Linear (V2 (V2))
import Test.HUnit ((@=?))

parse :: String -> Integer -> [V2 Integer]
parse str (pred -> repl) = lines >>> addGalaxies >>> Map.fromList >>> Map.filter (== '#') >>> Map.keys $ str
 where
  addGalaxies ls = zip (V2 <$> indexRows 0 ls <*> indexRows 0 (transpose ls)) (concat ls)

  indexRows c [] = []
  indexRows c (x : xs) | all (== '.') x, newC <- c + repl = newC : indexRows (newC + 1) xs
  indexRows c (x : xs) = (c) : indexRows (c + 1) xs

mandist (V2 x y) (V2 xx yy) = abs (x - xx) + abs (y - yy)

sumDistances :: [V2 Integer] -> Integer
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
  resA @=? 9957702
  let resB = sumDistances (parsed 1000000)
  print resB
  resB @=? 512240933238