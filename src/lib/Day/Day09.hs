module Day.Day09 where

import Control.Arrow ((>>>))
import Data.Foldable.Extra (Foldable (foldl'), sumOn')
import Test.HUnit ((@=?))

parse :: String -> [[Int]]
parse = lines >>> map (words >>> map read)

extrapolateNumbers :: (forall a. [a] -> [a]) -> [[Int]] -> Int
extrapolateNumbers transformer = sumOn' getExtrapolatedValue
 where
  getExtrapolatedValue = sumOn' head . takeWhile (any (/= 0)) . iterate (zipWith (-) <*> tail) . transformer

run :: String -> IO ()
run input = do
  let parsed = parse input
  let resA = extrapolateNumbers reverse parsed
  print resA
  resA @=? 2105961943
  let resB = extrapolateNumbers id parsed
  print resB
  resB @=? 1019
