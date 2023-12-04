module Day.Day04 (run) where

import Control.Arrow ((>>>))
import Control.Lens (Field2 (_2), over, taking)
import Data.IntSet (IntSet)
import Data.IntSet qualified as Set
import Data.List.Extra (splitOn, sumOn')
import Test.HUnit ((@=?))

parse :: String -> [(IntSet, IntSet)]
parse = lines >>> fmap parseLine
 where
  parseLine = dropWhile (/= ':') >>> drop 1 >>> splitOn " | " >>> fmap readInts >>> toTuple
  readInts = words >>> fmap read >>> Set.fromList
  toTuple [winning, yours] = (winning, yours)

countMatches :: (IntSet, IntSet) -> Int
countMatches = uncurry Set.intersection >>> Set.size

solveA :: [(IntSet, IntSet)] -> Int
solveA = sumOn' (countMatches >>> twoPow)
 where
  twoPow 0 = 0
  twoPow x = 2 ^ (x - 1)

solveB :: [(IntSet, IntSet)] -> Int
solveB = fmap (,1) >>> getCounts >>> sum
 where
  getCounts [] =
    []
  getCounts ((countMatches -> numMatches, count) : rest) =
    count : getCounts (over (taking numMatches (traverse . _2)) (+ count) rest)

run :: String -> IO ()
run input = do
  let parsed = parse input
  let resA = solveA parsed
  print resA
  resA @=? 22674
  let resB = solveB parsed
  print resB
  resB @=? 5747443
