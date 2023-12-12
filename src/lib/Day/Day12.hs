module Day.Day12 (run) where

import Control.Arrow (Arrow (first), (>>>))
import Control.Monad (void)
import Data.Function
import Data.List
import Data.List.Extra (splitOn, sumOn')
import Data.Monoid
import Debug.Trace
import Test.HUnit ((@=?))
import Text.RawString.QQ (r)
import Utils

data Rec = Op | Dmg | Unk deriving (Eq)

-- pattern

instance Show Rec where
  show x = case x of
    Op -> "."
    Dmg -> "#"
    Unk -> "?"

toRec '?' = Unk
toRec '#' = Dmg
toRec '.' = Op

data Tile = Stuck Int | Split | Un deriving (Eq, Read)

instance Show Tile where
  show x = case x of
    Split -> "."
    Stuck _ -> "#"
    Un -> "?"

parse :: String -> Int -> [([Tile], [Int])]
parse inp repl = lines >>> fmap parseLine $ inp
 where
  parseLine = words >>> (\[springs, nums] -> (pSprings springs, concat $ replicate repl $ pNums nums))
  pSprings = map toRec >>> group >>> concatMap (toTiles) >>> r

  r = concat . intersperse [Un] . replicate repl

  toTiles xs@(x : _) = case x of
    Op -> [Split]
    Dmg -> [Stuck $ length xs]
    Unk -> Un <$ xs

  pNums = splitOn "," >>> map (read @Int)

makeCombs :: [Maybe Int] -> _
makeCombs = traverse (maybe [(0, False), (1, False)] (pure . (,True)))

debug = False

traceLabd s a = if debug then traceLab s a else a
traceShowd s a = if debug then traceShow s a else a

-- doRec c gs tiles
-- \| traceShowd (c, gs, tiles) False =
--     undefined
doRec c (g : _) _
  | c > g = 0
doRec c (curGoals : goals) (Split : tiles)
  | c == 0 = doRec 0 (curGoals : goals) tiles
  | c == curGoals = doRec 0 goals tiles
  | otherwise = 0
doRec c goals (Stuck i : tiles) =
  doRec (c + i) goals tiles
doRec (id -> c) (curGoal : goals) (Un : tiles)
  | c == 0 = traceShowd (Un : tiles) useStuck + useSplit
  | c == curGoal = doRec 0 goals tiles
  | c < curGoal = useStuck
  | otherwise = traceShowd "WTF" 0 --
 where
  useStuck = traceLabd "stuck" $ doRec (c + 1) (curGoal : goals) tiles
  useSplit = traceLabd "split" $ doRec c (curGoal : goals) tiles
-- doRec c curG
doRec 0 [] tiles | all (\x -> x == Un || x == Split) tiles = 1
doRec 0 [] [] = 1
doRec c [g] [] | c == g = 1
doRec c gs ts = 0 -- & traceShowd (gs, ts)

-- doRec (curGoal : goals) (Stuck i : Un : tiles)
-- doRec [curGoal] [Stuck i] =
solveA repl inp = do
  let newInp = inp repl
  print "res"
  let res = sum $ fmap f $ newInp
  print $ res
 where
  f (tiles, goals) = trace ":)" doRec 0 goals tiles

-- check2 (nums, goal) = getRes nums
--   where
--     numSplits = count Nothing nums
-- check (nums, goal) = foldMap (\x -> if x == goal then Sum 1 else 0) $ getRes nums
-- getRes = makeCombs >>> fmap (group >>> traceShowdId >>> fmap (sumOn' fst) >>> filter (> 0))

solveB = id

testInput =
  (!! 0) $
    [r|???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1
|]
      : [r|?.### 1,3
|]
      : []

run :: String -> IO ()
run input = void $ do
  -- input <- putStrLn "#####    testInput   #####" >> pure testInput
  print input
  let parsed = parse input
  mprint $ parsed 1

  let resA = solveA 5 parsed
  resA

-- mapM_ (print . concatMap show . fst) $ parsed 5
-- mapM_ (print . snd) $ parsed 5

-- let resB = solveA 2 parsed
-- resB

-- resA @=? 1715
-- let resB = solveB parsed
-- print resB
-- resB @=? 1739
