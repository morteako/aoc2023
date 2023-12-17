module Day.Day16 (run) where

import Control.Applicative qualified as Set
import Control.Arrow ((>>>))
import Control.Monad.State
import Data.Foldable

import Data.Map.Strict qualified as Map hiding (filter)
import Data.Semigroup (Max (Max), Min (Min))

import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Set qualified as Set hiding (filter)
import Linear (V2 (..))
import Test.HUnit ((@=?))
import Utils (parseAsciiMap)

data Tile = Dot | Vert | Hor | Refl_V | ReflV_

type Grid = Map (V2 Int) Tile

parse :: String -> Grid
parse = parseAsciiMap toTile
 where
  toTile '.' = Just Dot
  toTile '|' = Just Vert
  toTile '-' = Just Hor
  toTile '\\' = Just Refl_V
  toTile '/' = Just ReflV_
  toTile x = error $ show x

data Dir = N | S | W | E deriving (Show, Eq, Ord, Enum, Bounded)

dirToVec :: Dir -> V2 Int
dirToVec x = case x of
  N -> V2 0 (-1)
  S -> V2 0 1
  E -> V2 1 0
  W -> V2 (-1) 0

isOne :: (Eq a) => [a] -> a -> Maybe a
isOne xs x = if elem x xs then Just x else Nothing

move :: Tile -> Dir -> [Dir]
move Dot dir = [dir]
move Vert (isOne [N, S] -> Just dir) = [dir]
move Hor (isOne [E, W] -> Just dir) = [dir]
move Vert (isOne [E, W] -> Just dir) = [N, S]
move Hor (isOne [N, S] -> Just dir) = [E, W]
move ReflV_ dir = [reflV_ dir]
 where
  reflV_ dir = case dir of
    N -> E
    E -> N
    W -> S
    S -> W
move Refl_V dir = [refl_V dir]
 where
  refl_V dir = case dir of
    N -> W
    W -> N
    E -> S
    S -> E

doMoves :: Map (V2 Int) Tile -> V2 Int -> Dir -> State (Set (V2 Int, Dir)) ()
doMoves grid curPos dir = case Map.lookup curPos grid of
  Nothing -> pure ()
  Just t -> do
    seen <- get
    if Set.member (curPos, dir) seen
      then pure ()
      else do
        modify (Set.insert (curPos, dir))
        let ds = move t dir
        for_ ds $ \d -> doMoves grid (dirToVec d + curPos) d

solveA :: Grid -> _
solveA grid = Set.size $ Set.map fst $ flip execState mempty $ doMoves grid (V2 0 0) E

edgesAndDirs :: Grid -> _
edgesAndDirs grid =
  fi (\(V2 x y) -> x == mx) E
    ++ fi (\(V2 x y) -> x == mxx) W
    ++ fi (\(V2 x y) -> y == my) S
    ++ fi (\(V2 x y) -> y == myy) N
 where
  fi p dir = map (,dir) $ filter p $ Map.keys grid
  (Min mx, Max mxx, Min my, Max myy) = Map.foldMapWithKey (\(V2 x y) _ -> (Min x, Max x, Min y, Max y)) grid

solveB :: Map (V2 Int) Tile -> Int
solveB grid =
  maximum
    . map (Set.size . Set.map fst)
    $ flip map (edgesAndDirs grid)
    $ flip execState mempty . uncurry (doMoves grid)

run :: String -> IO ()
run input = do
  let parsed = parse input
  let resA = solveA parsed
  print resA
  resA @=? 8098
  let resB = solveB parsed
  print resB
  resB @=? 8335
