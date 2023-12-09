module Day.Day09 where

import Control.Arrow ((>>>))
import Data.Foldable.Extra (Foldable (foldl'), sumOn')
import Test.HUnit ((@=?))

parse :: String -> [[Int]]
parse = lines >>> map (words >>> map read)

diffs :: (Num c) => [c] -> [c]
diffs xs = zipWith (-) xs (tail xs)

takeWhileIncludeNext :: (a -> Bool) -> [a] -> [a]
takeWhileIncludeNext p xs = case span p xs of
  (trues, falses) -> trues ++ take 1 falses

extrapolateNumbers :: (forall a. [a] -> [a]) -> [[Int]] -> Int
extrapolateNumbers transformer = sumOn' getExtrapolatedValue
 where
  getExtrapolatedValue = sumOn' head . takeWhileIncludeNext (any (/= 0)) . iterate diffs . transformer

run :: String -> IO ()
run input = do
  print input
  let parsed = parse input
  print parsed
  let resA = extrapolateNumbers reverse parsed
  print resA
  resA @=? 2105961943
  let resB = extrapolateNumbers id parsed
  print resB
  resB @=? 1019
