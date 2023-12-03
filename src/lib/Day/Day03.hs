module Day.Day03 (run) where

import Control.Arrow ((>>>))
import Control.Monad (void)
import Data.Char (isDigit)
import Data.Either (partitionEithers)
import Data.List
import Data.List.Extra (groupOn, nubOn, nubOrdOn, sumOn')
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Debug.Trace
import Linear (V2 (..))
import Test.HUnit ((@=?))
import Text.RawString.QQ

parse :: String -> (Map.Map (V2 Int) Char, [([V2 Int], Int)])
parse = lines >>> index >>> concatMap parseDigitOrSymbol >>> partitionEithers >>> makeMaps
 where
  index = zip [0 ..] >>> map (\(ind, line) -> zip (map (V2 ind) [0 ..]) line)
  makeMaps (syms, nums) = (Map.fromList syms, nums)
  parseDigitOrSymbol = groupOn (snd >>> isDigit) >>> foldMap getNums
  getNums g | isDigit char = [Right (inds, (read @Int digits))]
   where
    (inds, digits@(char : _)) = unzip g
  getNums g = fmap Left (filter (\(i, c) -> c /= '.') g)

neighs :: (Num a) => V2 a -> [V2 a]
neighs (V2 x y) = tail $ do
  x' <- [x, x - 1, x + 1]
  y' <- [y, y - 1, y + 1]
  [V2 x' y']

solveA :: (Map.Map (V2 Int) Char, [([V2 Int], Int)]) -> Int
solveA (syms, nums) = sumOn' snd $ filter hasAdjSymbol nums
 where
  hasAdjSymbol (inds, _) = any isAdjSym inds
  isAdjSym v2 = any (\xy -> Map.member xy syms) (neighs v2)

solveB :: (Map.Map (V2 Int) Char, [([V2 Int], Int)]) -> Int
solveB (Map.filter (== '*') -> syms, nums) = sum $ fmap product $ Map.filter (\xs -> length xs == 2) $ Map.mapWithKey (\k _ -> getAdjGears k) syms
 where
  numsMap = Map.fromList $ concatMap (\(inds, x) -> map (,(head inds, x)) inds) nums
  getAdjGears v2 = map snd $ nubOrdOn fst $ mapMaybe (flip Map.lookup numsMap) (neighs v2)

run :: String -> IO ()
run input = do
  let parsed = parse input
  let resA = solveA parsed
  print resA

  resA @=? 527446
  let resB = solveB parsed
  print resB
  resB @=? 73201705
