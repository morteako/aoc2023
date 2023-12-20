module Day.Day17 (run) where

import Data.Char
import Data.Graph.AStar (aStar)
import Data.Graph.Inductive (Gr, Graph (isEmpty, mkGraph), neighbors, sp, spLength)
import Data.HashSet qualified as HashSet
import Data.Hashable
import Data.List.Extra
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes, fromJust, mapMaybe)
import Data.Semigroup
import Data.Semigroup qualified as Semigroup
import Data.Set (Set)
import Data.Set qualified as Set hiding (fold)
import Data.Tuple.Extra (fst3, snd3)
import GHC.Generics
import Linear (V2 (..))
import Test.HUnit ((@=?))
import Utils

parse :: String -> _
parse = parseAsciiMap (Just . digitToInt)

data Dir = N | S | W | E deriving (Show, Eq, Ord, Enum, Bounded, Generic)

dirToVec :: Dir -> V2 Int
dirToVec x = case x of
  N -> V2 0 (-1)
  S -> V2 0 1
  E -> V2 1 0
  W -> V2 (-1) 0

d .+ v = dirToVec d + v

(Allowed d _) ..+ v = dirToVec d + v

data DirC = Allowed !Dir !Int deriving (Show, Eq, Ord, Generic)

instance Hashable (Dir)
instance Hashable (DirC)

turn N = (W, E)
turn S = (W, E)
turn W = (S, N)
turn E = (S, N)

solveA grid = do
  -- print $ r
  sum $ map (grid Map.!) $ map fst $ fromJust $ doAstar grid
 where
  doAstar grid = aStar neighbors dist heur isGoal (0, Allowed E 0)
   where
    inGrid = (flip Map.member grid)
    (ma, _) = Map.findMax grid

    neighbors (curPos, Allowed d i) =
      let
        (a, b) = turn d
        xs = (if i >= 3 then [] else [Allowed d (i + 1)]) ++ [Allowed a 1, Allowed b 1]
       in
        HashSet.fromList $ filter (inGrid . fst) $ map (\al -> (al ..+ curPos, al)) xs

    isGoal (curPos, _) = (curPos == ma)
    dist _ (curPos, _) = grid Map.! curPos
    heur _ = 0

solveB grid = do
  sum $ map getPathSum $ fromJust $ doAstar grid
 where
  getPathSum (curPos, acc, Allowed _ 4) = acc
  getPathSum (curPos, acc, Allowed d i) = grid Map.! curPos

  sum4 curPos d =
    case (take 4 . tail $ iterate (d .+) curPos) of
      ps -> case traverse (grid Map.!?) ps of
        Nothing -> Nothing
        Just xs -> Just (last ps, sum xs, Allowed d 4)

  inGrid = (flip Map.member grid)

  doAstar grid = aStar neighbors dist heur isGoal (0, 0, Allowed E 0)
   where
    (ma, _) = Map.findMax grid

    neighbors (curPos, acc, Allowed _ 0) =
      HashSet.fromList $ mapMaybe (sum4 curPos) [S, E]
    neighbors (curPos, acc, Allowed d i) =
      let
        (a, b) = turn d
        n = d .+ curPos
        next = if i >= 10 || not (inGrid n) then [] else [(n, acc + grid Map.! n, Allowed d (i + 1))]

        qs = next ++ mapMaybe (sum4 curPos) [a, b]
       in
        HashSet.fromList qs

    isGoal (curPos, _, _) = (curPos == ma)

    dist _ (curPos, acc, Allowed d 4) = acc
    dist _ (curPos, acc, Allowed d _) = grid Map.! curPos

    heur _ = 0

run :: String -> IO ()
run input = do
  let parsed = parse input

  let resA = solveA parsed
  print resA
  resA @=? resA

  let resB = solveB parsed
  print resB
  resB @=? 980