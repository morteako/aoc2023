module Day.Day21 (run) where

import Control.Arrow (Arrow ((&&&)), (>>>))
import Control.Monad (void)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Linear (V2 (..))
import Test.HUnit ((@=?))
import Text.RawString.QQ (r)
import Utils

parse = parseAsciiMap f >>> (getStart &&& fixMap)
 where
  f '.' = Just Dot
  f '#' = Just Rock
  f 'S' = Just Start

  getStart = Map.toList >>> filter ((== Start) . snd) >>> head >>> fst

  fixMap = Map.filter (/= Rock) >>> Map.keysSet

data Tile = Dot | Rock | Start deriving (Show, Eq)

dirs =
  [ V2 0 (-1)
  , V2 0 1
  , V2 1 0
  , V2 (-1) 0
  ]

-- mov xs = Set.map (

move :: Set.Set (V2 Int) -> Set.Set (V2 Int) -> Set.Set (V2 Int)
move grid xs = Set.intersection grid $ Set.unions (map (\d -> Set.map (+ d) xs) dirs)

solveA (start, grid) = Set.size $ last $ take 65 $ iterate (move grid) (Set.singleton start)

run :: String -> IO ()
run input = void $ do
  let parsed = parse input
  let resA = solveA parsed
  print resA
  resA @=? 3782
