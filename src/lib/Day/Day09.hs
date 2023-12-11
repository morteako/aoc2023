module Day.Day09 where

import Control.Arrow ((>>>))
import Data.Foldable.Extra (sumOn')
import Data.Maybe qualified as Maybe
import Data.Monoid
import Test.HUnit ((@=?))

parse :: String -> [[Int]]
parse = lines >>> map (words >>> map read)

extrapolateNumbers transformer = sumOn' getExtrapolatedValue
 where
  getExtrapolatedValue = foldMap (foldMap Sum . Maybe.listToMaybe) . iterate (zipWith (-) <*> tail) . transformer

run :: String -> IO ()
run input = do
  let parsed = parse input
  let resA = extrapolateNumbers reverse parsed
  print resA
  resA @=? 2105961943
  let resB = extrapolateNumbers id parsed
  print resB
  resB @=? 1019
