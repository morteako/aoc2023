module Day.Day03 (run) where

import Control.Arrow ((>>>))
import Control.Monad (void)
import Data.Char (isDigit)
import Data.Either (partitionEithers)
import Data.List
import Data.List.Extra (groupOn, nubOn, nubOrdOn, sumOn')
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Linear (V2 (..))
import Test.HUnit ((@=?))

parse :: String -> (Map.Map (V2 Int) Char, [([V2 Int], Int)])
parse = lines >>> index >>> concatMap parseDigitOrSymbol >>> partitionEithers >>> makeMaps
 where
  index = zip [0 ..] >>> map (\(ind, line) -> zip (map (V2 ind) [0 ..]) line)
  makeMaps (syms, nums) = (Map.fromList syms, nums)
  parseDigitOrSymbol = groupOn (snd >>> isDigit) >>> getNums
  getNums (g : gs) | (_, char) : rest <- g, isDigit char = Right (inds, (read @Int digits)) : getNums gs
   where
    (inds, digits) = unzip g
  getNums (g : gs) = fmap Left (filter (\(i, c) -> c /= '.') g) ++ getNums gs
  getNums [] = []

dirs :: (Num a) => V2 a -> [V2 a]
dirs (V2 x y) = tail $ do
  x' <- [x, x - 1, x + 1]
  y' <- [y, y - 1, y + 1]
  [V2 x' y']

solveA :: (Map.Map (V2 Int) Char, [([V2 Int], Int)]) -> Int
solveA (syms, nums) = sumOn' snd $ filter hasAdjSymbol nums
 where
  hasAdjSymbol (inds, _) = any isAdjSym inds
  isAdjSym v2 = any (\xy -> Map.member xy syms) (dirs v2)

solveB :: (Map.Map (V2 Int) Char, [([V2 Int], Int)]) -> Int
solveB (Map.filter (== '*') -> syms, nums) = sum $ fmap product $ Map.filter (\xs -> length xs == 2) $ Map.mapWithKey (\k _ -> getAdjGears k) syms
 where
  numsMap = Map.fromList $ concatMap (\(inds, x) -> map (,(head inds, x)) inds) nums
  getAdjGears v2 = map snd $ nubOrdOn fst $ mapMaybe (flip Map.lookup numsMap) (dirs v2)

run :: String -> IO ()
run input = void $ do
  let parsed = parse input
  let resA = solveA parsed
  resA @=? 527446
  let resB = solveB parsed
  resB @=? 73201705
