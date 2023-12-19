module Day.Day17 (run) where

import Control.Arrow ((>>>))
import Control.Lens
import Control.Monad (guard, void)
import Control.Monad.State
import Data.Char
import Data.Coerce
import Data.Foldable
import Data.Graph.Inductive (Gr, Graph (isEmpty, mkGraph), sp, spLength)
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
import GHC.Records
import Linear (V2 (..))
import Test.HUnit ((@=?))
import Text.RawString.QQ (r)
import Utils

parse :: String -> [[(V2 Int, Int)]]
parse = lines >>> imap (\i line -> imap (\j c -> (V2 j i, digitToInt c)) line)

data Dir = N | S | W | E deriving (Show, Eq, Ord, Enum, Bounded)

dirToVec :: Dir -> V2 Int
dirToVec x = case x of
  N -> V2 0 (-1)
  S -> V2 0 1
  E -> V2 1 0
  W -> V2 (-1) 0

d .+ v = dirToVec d + v

data OneThree = Allowed Int | TooLong

instance HasField "inc" OneThree OneThree where
  getField x = case x of
    TooLong -> TooLong
    Allowed 3 -> TooLong
    Allowed x -> Allowed (x + 1)

-- doMoves grid curPos dir TooLong = []
-- doMoves grid curPos dir (Allowed i) = case Map.lookup curPos grid of
--   Nothing -> _
--   Just val -> _

dtraceShow s a = if debug then traceShow s a else a
dtraceLab s a = if debug then traceLab s a else a

debug = False

toMon (v2, cost) = (Last v2, Sum cost)

makeGraph :: [((V2 Int, V2 Int), Int)] -> _
makeGraph xs = (mkGraph @Gr lnodes ledges, 0, Set.size nodes - 1, fromInd)
 where
  lnodes = fmap (\n -> (getInd n, n)) $ Set.toList nodes
  ledges = [(getInd a, getInd b, c) | ((a, b), c) <- xs]
  nodes = foldMap (\((a, b), _) -> Set.singleton a <> Set.singleton b) xs
  getInd = flip Set.findIndex nodes
  fromInd = flip Set.elemAt nodes

getRowsAndColumns :: _ -> [[(V2 Int, Int)]] -> [((V2 Int, V2 Int), Int)]
getRowsAndColumns goal xs = Map.toList $ Map.fromList $ do
  let rs = concatMap getRow xs ++ concatMap getRow (map reverse xs)
  let trans = transpose xs
  let cs = concatMap getRow trans ++ concatMap getRow (map reverse trans)
  (ra, rb, rcost) <- rs
  (ca, cb, ccost) <- cs
  let cost = rcost + ccost
  if
    -- \| ra == ca -> [((rb, cb), cost), ((cb, rb), cost)]
    | rb == ca -> [((ra, cb), cost)]
    | rb == goal -> [((ra, rb), rcost)]
    | cb == goal -> [((ca, cb), ccost)]
    -- ADD CASE? or safe
    | otherwise -> []

fc = flip const

getRow :: [(V2 Int, Int)] -> [(V2 Int, V2 Int, Int)]
getRow = id . coerce . concatMap f . tails . map toMon
 where
  f [] = []
  f xs@((Last x, _) : _) = map (\(l, c) -> (x, l, c)) . drop 3 . scanl1 (<>) . drop 1 . take 11 $ xs

solveA grid = do
  -- print grid
  -- printlab "row" $ getRow $ map (\x -> (V2 x x, x)) [1, 2, 3, 4, 5, 6]
  let t = fst $ last $ last grid
  print "-------"
  print t
  print "-------"

  let alls = getRowsAndColumns t grid
  print $ length alls

  let (graph, start, end, fromInd) = makeGraph alls
  print $ isEmpty graph
  printlab "sp" $ spLength start end graph
  let Just path = fmap (map fromInd) $ sp start end graph
  printlab "sp" $ fmap (map fromInd) $ sp start end graph

  print "------"

  let zips = zip path (tail path)
  for_ zips $ \i -> do
    putStr (show i ++ " : -------------- ")
    print $ lookup i alls
    -- mapM_ print $ getRowsAndColumns $ transpose grid
    -- printlab "getevmoves" getEWmoves
 where

solveB = id

-- [(V2 0 0, V2 0 1, 1)]

testInput =
  [r|111111111111
999999999991
999999999991
999999999991
999999999991|]

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

-- _ 4 1 1 5 4 5 3
-- 8 - 2

-- 2413432311323
-- 3215453535623
-- 3255245654254
-- 3446585845452
-- 4546657867536
-- 1438598798454
-- 4457876987766
-- 3637877979653
-- 4654967986887
-- 4564679986453
-- 1224686865563
-- 2546548887735
-- 4322674655533

-- 2>>34^>>>1323
-- 32v>>>35v5623
-- 32552456v>>54
-- 3446585845v52
-- 4546657867v>6
-- 14385987984v4
-- 44578769877v6
-- 36378779796v>
-- 465496798688v
-- 456467998645v
-- 12246868655<v
-- 25465488877v5
-- 43226746555v>

-- .4134.2311323
-- 32.54535.5623
-- 325524565.254
-- 3446585845452
-- 4546657867.36
-- 1438598798454
-- 4457876987766
-- 36378779796.3
-- 4654967986887
-- 4564679986453
-- 122468686556.
-- 25465488877.5
-- 432267465553.

run :: String -> IO ()
run input = void $ do
  input <- putStrLn "#####    testInput   #####" >> pure testInput
  -- print input
  let grid = parseAsciiMap (Just . digitToInt) input
  -- printV2Map $ fmap show grid
  -- print "--------"
  let p = [V2 0 0, V2 2 1, V2 5 0, V2 8 1, V2 9 2, V2 10 4, V2 11 7, V2 12 10, V2 11 11, V2 12 12]
  -- printV2Map $ Map.mapWithKey (\k v -> if elem k p then "." else show v) grid

  let parsed = parse input
  -- mapM_ print parsed

  let resA = solveA parsed
  resA
  let p = [V2 0 0, V2 8 5, V2 18 1, V2 27 5, V2 35 1, V2 45 5, V2 50 0, V2 59 4, V2 69 0, V2 78 4, V2 86 0, V2 96 4, V2 106 9, V2 114 13, V2 122 17, V2 126 24, V2 132 34, V2 136 44, V2 140 54, V2 136 63, V2 132 72, V2 136 82, V2 140 92, V2 136 100, V2 140 110, V2 136 120, V2 140 130, V2 140 140]
  print $ zipWith (-) p (tail p)

-- printFasit

-- print "hopp"

-- print $ fold [Just (Min 1), Nothing]

-- resA @=? 1715
-- let resB = solveB parsed
-- print resB
-- resB @=? 1739
