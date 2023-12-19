module Day.Day17 (run) where

import Control.Arrow ((>>>))
import Control.Lens
import Control.Monad (guard, void)
import Control.Monad.State
import Data.Char
import Data.Coerce
import Data.Foldable
import Data.Graph.AStar
import Data.Graph.Inductive (Gr, Graph (isEmpty, mkGraph), sp, spLength)
import Data.HashSet qualified as HashSet
import Data.Hashable
import Data.List.Extra
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes, mapMaybe)
import Data.Semigroup
import Data.Semigroup qualified as Semigroup
import Data.Set (Set)
import Data.Set qualified as Set hiding (fold)
import Data.Tuple.Extra (fst3, snd3)
import Debug.Trace
import GHC.Generics
import GHC.Records
import Linear (V2 (..))
import Test.HUnit ((@=?))
import Text.RawString.QQ (r)
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

getDirs :: DirC -> [DirC]
getDirs (Allowed d i) =
  let
    (a, b) = turn d
    xs = [Allowed a 1, Allowed b 1]
   in
    (if i >= 3 then xs else (Allowed d (i + 1)) : xs)

type St = State Int

doAstar grid = aStarM @St _graph _dist _heur _goal (pure $ (0, Allowed E 0))
 where
  inGrid = (flip Map.member grid)
  (ma, _) = Map.findMax grid

  _graph (curPos, Allowed d i) = do
    let
      (a, b) = turn d
      xs = (if i >= 3 then [] else [Allowed d (i + 1)]) ++ [Allowed a 1, Allowed b 1]
     in
      pure $ HashSet.fromList $ filter (inGrid . fst) $ map (\al -> (al ..+ curPos, al)) xs

  _goal (curPos, _) = pure (curPos == ma)
  _dist _ (curPos, _) = pure $ grid Map.! curPos
  _heur _ = pure 0

dtraceShow s a = if debug then traceShow s a else a
dtraceLab s a = if debug then traceLab s a else a

debug = False

solveA grid = do
  print "solveA"
  let a = doAstar grid
  let (Just p, _) = flip runState 0 a
  -- print $ r
  print $ sum $ map (grid Map.!) $ map fst p
  -- print $ flip runState mempty $ findMin grid
 where

solveB = id

-- [(V2 0 0, V2 0 1, 1)]

testInput =
  [r|11
11|]

testInputOrg =
  [r|2413432311323
3215453535623
3255245654254
3446585845452
4546657867536
1438598798454
4457876987766
3637877979653
4654967986887
4564679986453
1224686865563
2546548887735
4322674655533
|]

run :: String -> IO ()
run input = void $ do
  -- input <- putStrLn "#####    testInput   #####" >> pure testInputOrg
  -- print input
  let grid = parseAsciiMap (Just . digitToInt) input
  -- printV2Map $ fmap show grid
  -- print "--------"
  -- let p = [V2 0 0, V2 2 1, V2 5 0, V2 8 1, V2 9 2, V2 10 4, V2 11 7, V2 12 10, V2 11 11, V2 12 12]
  -- printV2Map $ Map.mapWithKey (\k v -> if elem k p then "." else show v) grid

  let parsed = parse input

  printV2Map parsed

  solveA parsed

-- print $ concatMap getDirs $ concatMap getDirs $ getDirs (Allowed S 0)

-- mapM_ print parsed

-- let resA = solveA parsed
-- resA
-- let p = [V2 0 0, V2 8 5, V2 18 1, V2 27 5, V2 35 1, V2 45 5, V2 50 0, V2 59 4, V2 69 0, V2 78 4, V2 86 0, V2 96 4, V2 106 9, V2 114 13, V2 122 17, V2 126 24, V2 132 34, V2 136 44, V2 140 54, V2 136 63, V2 132 72, V2 136 82, V2 140 92, V2 136 100, V2 140 110, V2 136 120, V2 140 130, V2 140 140]
-- print $ zipWith (-) p (tail p)

-- printFasit

-- print "hopp"

-- print $ fold [Just (Min 1), Nothing]

-- resA @=? 1715
-- let resB = solveB parsed
-- print resB
-- resB @=? 1739
