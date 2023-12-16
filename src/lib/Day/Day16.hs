module Day.Day16 (run) where

import Control.Applicative qualified as Set
import Control.Arrow ((>>>))
import Control.Monad (void)
import Control.Monad.State
import Data.Foldable
import Data.Map
import Data.Map qualified as Map
import Data.Semigroup
import Data.Set
import Data.Set qualified as Set
import Data.Traversable
import Linear hiding (E)
import Test.HUnit ((@=?))
import Text.RawString.QQ (r)
import Utils

data Tile = Dot | Vert | Hor | Refl_V | ReflV_

instance Show Tile where
  show Dot = "."
  show Vert = "|"
  show Hor = "-"
  show Refl_V = "\\"
  show ReflV_ = "/"

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

pattern NE d <- ((\x -> if x == N || x == E then Just x else Nothing) -> Just d)
pattern SW d <- ((\x -> if x == S || x == W then Just x else Nothing) -> Just d)

pattern J x = Just x

isOne xs x = if elem x xs then Just x else Nothing

-- /
reflV_ dir = case dir of
  N -> E
  E -> N
  W -> S
  S -> W

-- \
refl_V dir = case dir of
  N -> W
  W -> N
  E -> S
  S -> E

move Dot dir = [dir]
move Vert (isOne [N, S] -> J dir) = [dir]
move Hor (isOne [E, W] -> J dir) = [dir]
move Vert (isOne [E, W] -> J dir) = [N, S]
move Hor (isOne [N, S] -> J dir) = [E, W]
move ReflV_ dir = [reflV_ dir]
move Refl_V dir = [refl_V dir]

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
edgesAndDirs grid = (x, xx, y, yy)
 where
  (Min x, Max xx, Min y, Max yy) = Map.foldMapWithKey (\(V2 x y) _ -> (Min x, Max x, Min y, Max y)) grid

solveB grid = es
 where
  es = edgesAndDirs grid

testInput =
  [r|.|...\....
|.-.\.....
.....|-...
........|.
..........
.........\
..../.\\..
.-.-/..|..
.|....-|.\
..//.|....|]

-- >|<<<\....

{-
 |v-.\^....
 .v...|->>>
 .v...v^.|.
 .v...v^...
 .v...v^..\
 .v../2\\..
 <->-/vv|..
 .|<<<2-|.\
 .v//.|.v..
-}
run :: String -> IO ()
run input = void $ do
  -- input <- putStrLn "#####    testInput   #####" >> pure testInput
  -- print input
  let parsed = parse input
  -- mprint parsed
  -- printV2Map parsed
  let resA = solveA parsed
  print resA

-- print $ move (V2 7 6) ,N,[W]
-- print 1

-- resA @=? 1715
-- let resB = solveB parsed
-- print resB
-- resB @=? 1739
