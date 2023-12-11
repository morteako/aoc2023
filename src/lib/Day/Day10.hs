module Day.Day10 (run) where

import Control.Arrow ((>>>))
import Control.Lens
import Control.Monad (void)
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
import Linear hiding (E)
import Test.HUnit ((@=?))
import Text.RawString.QQ (r)
import Utils

data Dir = N | S | W | E deriving (Show, Eq, Ord, Enum, Bounded)

data Tile = Pipe Pipe | Bend Bend | Start | Dot deriving (Eq, Ord)

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
  N -> V2 0 (-1)
  E -> V2 1 0
  W -> V2 (-1) 0
  S -> V2 0 1

pattern a :# b = (a, b)

nextWalk d x = case d :# x of
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

-- N :# Start -> undefined

walk grid startDir startPos = go startDir startPos 0
 where
  go d pos c = case grid ! pos of
    -- x | flip const (d, pos, x, nextWalk d x) (c > 20) -> -10
    Start | c > 0 -> []
    tile -> case (nextWalk d tile, tile) of
      (Just newDir, Bend _) -> Left pos : go newDir (dirToVec newDir + pos) (c + 1)
      (Just newDir, _) -> Right pos : go newDir (dirToVec newDir + pos) (c + 1)
      -- (Just newDir, _) -> go newDir (dirToVec newDir + pos) (c + 1)
      _ -> error $ show $ (d, pos, tile, nextWalk d tile)

getX (V2 x _) = x
getY (V2 _ y) = y

zipSelf = zip <*> tail

-- shoelace points = abs $ sum (zipWith calc xs ys) * 0.5
--  where
--   calc (x, x1) (y, y1) = (((y + y1) * (x - x1)))
--   ps = (points ++ take 1 points)
--   xs = zipSelf $ map getX ps
--   ys = zipSelf $ map getY ps

parse :: String -> Map.Map (V2 Int) Tile
parse = parseAsciiMap toTile . unlines . filter (not . null) . lines

--

solveA :: Map.Map (V2 Int) Tile -> _
solveA grid = length (walk grid startDir startPos) `div` 2
 where
  startPos = id $ fst $ Map.findMin $ Map.filter (== Start) grid
  startDir = head $ catMaybes $ traceShowId $ mapMaybe (\d -> fmap (nextWalk d) (grid !? (startPos + dirToVec d))) [minBound .. maxBound]

---- A

-- checkInside :: Map.Map (V2 Int) Tile -> Map.Map (V2 Int) Tile -> (V2 Int) -> _
-- checkInside grid path startPos = odd $ length $ traceOn (\g -> show (startPos, map (concatMap show) g, length g)) $ groupBy grouper $ filter (/= Pipe Hor) $ mapMaybe look points
--  where
--   look (x) = grid !? x
--   points = takeWhile (flip Map.member grid) . iterate (over _x succ) $ startPos

-- grouper Dot Dot = True
-- grouper Dot _ = False
-- grouper _ Dot = False
-- -- grouper _ (Pipe Vert) = False
-- grouper _ _ = True

-- grouper (Pipe Hor) (Bend _) = True
-- grouper (Bend _) (Pipe Hor) = True
-- grouper (Bend _) Start = True
-- grouper Start (Bend _) = True

-- SQUUUEZE

-- canReachExitState :: Map.Map (V2 Int) a -> Set (V2 Int) -> V2 Int -> State (Set (V2 Int)) Bool
-- canReachExitState grid path pos =
--   do
--     v <- get

-- (FALSES, TURES)

dirs = [N, S, E, W]

type RInt = Ratio Int

canReachExitState :: Map (V2 RInt) a -> Set (V2 RInt) -> Set (V2 RInt) -> Set (V2 RInt) -> (Set (V2 RInt), Set (V2 RInt))
-- canReachExitState grid path vis poses | traceShow ("vis", vis, "poses", poses) False = undefined
canReachExitState grid path vis poses | Set.null poses = (vis, Set.empty)
canReachExitState grid path vis poses | any (flip Map.notMember grid) poses = (Set.empty, vis <> poses)
canReachExitState grid path vis poses = do
  -- let newPoses =  [N, S, E, W]
  let newVis = vis <> poses
  let addNew acc newP = if Set.notMember newP path && Set.notMember newP newVis then Set.insert newP acc else acc

  -- let q = foldl' addNew poses $ m <$>
  let newPs = Set.unions $ fmap (\d -> Set.map (dirToVec d +) poses) dirs

  let newPoses = traceLab "newposes" $ foldl' addNew Set.empty newPs

  canReachExitState grid path newVis (newPoses)

solveB :: Map.Map (V2 Int) Tile -> _
solveB grid = do
  -- print bends
  -- print "BEND" >> print bends
  -- print $ checkInside grid bends (
  -- print dots
  -- print "che" >> print (Set.size $ Set.filter (checkInside grid bends) dots)
  -- print $ length dots
  -- let res =
  --       fst $
  --         foldl'
  --           ( \(falses, trues) d ->
  --               if Set.member d falses
  --                 then (falses, trues)
  --                 else
  --                   if Set.member d trues
  --                     then (falses, trues)
  --                     else (falses, trues) <> check d
  --           )
  --           mempty
  --           dots

  print "bendmap"
  printV2Map $ bendMap <> Map.map (const Dot) grid
 where
  -- print $ Set.size res

  addHalfDots = Map.foldMapWithKey getDots ratioGrid

  ratioGrid = Map.mapKeys (fmap toRational) grid

  getDots k v = fmap (flip Map.singleton v) $ case v of
    Bend NE -> [k]
    Bend NW -> [k]
    Bend SE -> [k]
    Bend SW -> [k]
    Pipe Hor -> k : fmap (+ k) [halfDirToVec N, halfDirToVec S]
    Pipe Vert -> k : fmap (+ k) [halfDirToVec W, halfDirToVec E]
    Dot -> [k]
  -- Start -> _

  -- check p = canReachExitState grid bends Set.empty (Set.singleton p)
  dots = Map.keys $ Map.difference grid bendMap

  bendMap = Map.fromList $ map swap $ walkMod grid startDir startPos
  bends = Map.keysSet $ bendMap

  startPos = traceShowId $ fst $ Map.findMin $ Map.filter (== Start) grid
  startDir = S -- head $ catMaybes $ mapMaybe (\d -> fmap (nextWalk d) (grid !? (startPos + dirToVec d))) [minBound .. maxBound]

halfDirToVec = fmap ((1 % 2) *) . dirToVec

walkMod grid startDir startPos = (Start, startPos) : go startDir startPos 0
 where
  go d pos c = case grid ! pos of
    -- x | flip const (d, pos, x, nextWalk d x) (c > 20) -> -10
    Start | c > 0 -> []
    tile -> case (nextWalk d tile, tile) of
      (Just newDir, b) -> (b, pos) : go newDir (dirToVec newDir + pos) (c + 1)
      -- (Just newDir, p@Pipe{}) -> (p, pos) : go newDir (dirToVec newDir + pos) (c + 1)
      -- (Just newDir, _) -> go newDir (dirToVec newDir + pos) (c + 1)
      _ -> error $ show $ (d, pos, tile, nextWalk d tile)

-- raycast

-- f

testInput =
  (!! 1)
    [ [r|
S-7
|.|
L-J|]
    , [r|.....
.S--7.
.|..|.
.L7FJ.
..LJ.|]
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
run :: String -> IO ()
run input = void $ do
  input <- putStrLn "#####    testInput   #####" >> pure testInput
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
