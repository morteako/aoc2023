module Day.Day10 (run) where

import Control.Arrow ((>>>))
import Control.Lens
import Control.Monad (mfilter, void)
import Control.Monad.State
import Data.Either (lefts)
import Data.List hiding (groupBy)
import Data.List.GroupBy
import Data.Map (Map, (!), (!?))
import Data.Map qualified as Map
import Data.Maybe
import Data.Ratio
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Tuple
import Data.Vector.Internal.Check (checkIndex)
import Debug.Trace
import Linear hiding (E, trace)
import Test.HUnit ((@=?))
import Text.RawString.QQ (r)
import Utils

data Dir = N | S | W | E deriving (Show, Eq, Ord, Enum, Bounded)

data Tile = Pipe Pipe | Bend Bend | Start | Dot | FakeDot | Block deriving (Eq, Ord)

data Pipe = Vert | Hor deriving (Eq, Ord)
data Bend = NE | NW | SW | SE deriving (Eq, Ord, Show)

instance Show Tile where
  show x = case x of
    Pipe Vert -> "|"
    Pipe Hor -> "-"
    Bend NE -> "L"
    Bend NW -> "J"
    Bend SW -> "7"
    Bend SE -> "F"
    Start -> "S"
    Dot -> "."

-- FakeDot -> "*"
-- Block -> "#"

toTile x = case x of
  '|' -> Just $ Pipe Vert
  '-' -> Just $ Pipe Hor
  'L' -> Just $ Bend NE
  'J' -> Just $ Bend NW
  '7' -> Just $ Bend SW
  'F' -> Just $ Bend SE
  'S' -> Just $ Start
  '.' -> Just Dot

dirToVec :: (Num a) => Dir -> V2 a
dirToVec x = case x of
  N -> V2 0 (-2)
  E -> V2 2 0
  W -> V2 (-2) 0
  S -> V2 0 2

halfDirToVec :: Dir -> V2 Int
halfDirToVec = fmap (`div` 2) . dirToVec

pattern a :# b = (a, b)

nextWalkMay d x = case d :# x of
  N :# Pipe Vert -> Just N
  S :# Pipe Vert -> Just S
  W :# Pipe Hor -> Just W
  E :# Pipe Hor -> Just E
  S :# Bend NE -> Just E
  S :# Bend NW -> Just W
  N :# Bend SW -> Just W
  N :# Bend SE -> Just E
  W :# Bend NE -> Just N
  E :# Bend NW -> Just N
  E :# Bend SW -> Just S
  W :# Bend SE -> Just S
  d :# Start -> Just d
  _ -> Nothing

nextWalk d x = case nextWalkMay d x of
  Nothing -> error $ show (d, x)
  Just x -> x

parse :: String -> Map.Map (V2 Int) Tile
parse = parseAsciiMap toTile . unlines . filter (not . null) . lines

debug = False

traceLabD s a = if debug then traceLab s a else a
traceD s a = if debug then trace s a else a

dirs = [N, S, E, W]

isOutside g = \x -> x < fst (Map.findMin g) || x > fst (Map.findMax g)

canReachExitState :: Map (V2 Int) Tile -> Set (V2 Int) -> Set (V2 Int) -> Set (V2 Int) -> (Set (V2 Int), Set (V2 Int))
-- canReachExitState grid path vis poses | traceShow ("vis", vis, "poses", poses) False = undefined
canReachExitState grid path vis poses
  | Set.null poses =
      (vis, Set.empty)
canReachExitState grid path vis poses
  | any (isOutside grid) poses =
      (Set.empty, vis <> poses)
canReachExitState grid path vis poses = do
  -- let newPoses =  [N, S, E, W]

  let newVis = vis <> traceLabD "poses" poses
  let addNew acc newP = if Set.notMember newP path && Set.notMember newP newVis then Set.insert newP acc else acc

  -- let q = foldl' addNew poses $ m <$>
  let newPs = traceLabD "newPs" $ Set.unions $ fmap (\d -> Set.map (halfDirToVec d +) poses) dirs

  let newPosesDebug = Set.map (\x -> (x, grid !? x)) $ foldl' addNew Set.empty newPs
  let newPoses = traceD ("newposes" ++ show newPosesDebug) $ foldl' addNew Set.empty newPs

  canReachExitState grid path newVis (newPoses)


fixStart (Map.mapKeys (* 2) -> grid) = (newGrid, startDir, startPos)
 where
  startPos = traceShowId $ fst $ Map.findMin $ Map.filter (== Start) grid
  -- startDirs = catMaybes $ mapMaybe (\d -> fmap (nextWalkMay d) (grid !? (startPos + dirToVec d))) [minBound .. maxBound]
  startDirs = mapMaybe (\d -> d <$ fmap (nextWalkMay d) (mfilter (/= Dot) $ grid !? (startPos + dirToVec d))) [minBound .. maxBound]

  (newGrid,startDir) = (Map.insert startPos (Pipe Hor) grid, W)
  -- (newGrid, startDir) = case sort startDirs of
  --   [S, E] -> (Map.insert startPos (Bend SE) grid, S)
  --   [S, W] -> (Map.insert startPos (Bend SW) grid, S)
  --   [N, W] -> (Map.insert startPos (Bend NW) grid, N)
  --   [N, E] -> (Map.insert startPos (Bend NE) grid, N)
  --   _ -> error $ show $ sort startDirs

solveB :: Map.Map (V2 Int) Tile -> _
solveB (fixStart -> (grid, startDir, startPos)) = do
  -- printV2Map moreGrid
  -- mprint moreGrid
  -- printLab "bends" bends

  let allEnclodesDots =
        fst $
          foldl'
            ( \(falses, trues) d ->
                if Set.member d falses
                  then (falses, trues)
                  else
                    if Set.member d trues
                      then (falses, trues)
                      else (falses, trues) <> check d
            )
            mempty
            dots
  -- printLab "lengthAll" $ length allEnclodesDots
  printLab "lengthFilter" $ length $ Set.filter (\(V2 x y) -> mod x 2 == 0 && mod y 2 == 0) $ allEnclodesDots
 where
  -- moreGrid = Map.unionsWith combTile $ [grid] ++ Map.foldMapWithKey (\k v -> [getHalfDots k v]) bendsMap

  -- USE CORRECT BENDS
  check p = canReachExitState grid bendsSet Set.empty (Set.singleton p)
  dots = traceLabD "Dots" $ Map.keys $ Map.difference grid bendsMap

  bendsSet = Set.fromList . map fst $ Map.toList bendsMap
  bendsMap = Map.fromList $ map (,()) bends
  bends = walkMod grid startDir startPos

-- _ = zip bendsV2

walkMod grid startDir startPos = (halfDirToVec startDir + startPos) : go startDir (dirToVec startDir + startPos)
 where
  go d pos =
    let tile = grid ! pos
     in case nextWalk d tile of
          _ | pos == startPos -> [pos]
          -- x | flip const (d, pos, x, nextWalk d x) (c > 20) -> -10
          newDir -> (pos) : (halfDirToVec newDir + pos) : go newDir (dirToVec newDir + pos)

-- (Just newDir, p@Pipe{}) -> (p, pos) : go newDir (dirToVec newDir + pos) (c + 1)
-- (Just newDir, _) -> go newDir (dirToVec newDir + pos) (c + 1)
-- _ -> error $ show $ (d, pos, tile, nextWalk d tile)

-- raycast

-- f

testInput =
  (!! 5)
    [ [r|
S-7
|.|
L-J|]
    , [r|
S7
||
LJ|]
    , [r|
......
.S--7.
.|..|.
.L7FJ.
..LJ..|]
    , [r|-L|F7
7S-7|
L|7||
-L-J|
L|-JF|]
    , [r|
S-------7
|F-----7|
||.....||
||.....||
|L-7.F-J|
|..|.|..|
L--J.L--J|]
    , [r|
S------7
|F----7|
||....||
||....||
|L-7F-J|
|..||..|
L--JL--J|]
    , [r|
FSF7.
||||.
|LJ|.
L--J.|]
    , [r|
FF7FSF7F7F7F7F7F---7
L|LJ||||||||||||F--J
FL-7LJLJ||||||LJL-77
F--JF--7||LJLJ7F7FJ-
L---JF-JLJ.||-FJLJJ7
|F|F-JF---7F7-L7L|7|
|FFJF7L7F-JF7|JL---7
7-L-JL7||F7|L7F-7F7|
L.L7LFJ|||||FJL7||LJ
L7JLJL-JLJLJL--JLJ.L|]
    , [r|
.F----7F7F7F7F-7....
.|F--7||||||||FJ....
.||.FJ||||||||L7....
FJL7L7LJLJ||LJ.L-7..
L--J.L7...LJS7F-7L7.
....F-J..F7FJ|L7L7L7
....L7.F7||L7|.L7L7|
.....|FJLJ|FJ|F7|.LJ
....FJL-7.||.||||...
....L---J.LJ.LJLJ...|]
    ]

-- L7JLJL-JLJLJL--JLJ.L
-- L.L7LFJ|||||FJL7||LJ
-- 7-L-JL7||F7|L7F-7F7|

{- | FFJF7L7F-JF7**L---7
 |F|F-JF---7***L7L|7|
 L---JF-JLJ****FJLJJ7
 F--JF--7||LJLJ7F7FJ-
 FL-7LJLJ||||||LJL-77
 L|LJ||||||||||||F--J
 FF7FSF7F7F7F7F7F---7
-}
printLab s x = putStr (s ++ " ") >> print x

run :: String -> IO ()
run input = void $ do
  -- input <- putStrLn "#####    testInput   #####" >> pure testInput
  -- mprint input
  let parsed = parse input
  mprint parsed
  printV2Map parsed

  -- let resA = solveA parsed
  -- print resA

  -- resA @=? 1715
  print "DEL2"
  let resB = solveB parsed
  resB

-- resB @=? 1739
