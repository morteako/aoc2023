{-# LANGUAGE TemplateHaskell #-}

module Day.Day12 (run) where

import Control.Arrow (Arrow (first), (>>>))
import Control.Monad (void)
import Data.Function.Memoize
import Data.List
import Data.List.Extra (splitOn, sumOn')
import Test.HUnit ((@=?))

data Rec = Op | Dmg | Unk deriving (Eq)

instance Show Rec where
  show x = case x of
    Op -> "."
    Dmg -> "#"
    Unk -> "?"

toRec '?' = Unk
toRec '#' = Dmg
toRec '.' = Op

data Tile = Stuck Int | Split | Un deriving (Eq)

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

deriveMemoizable ''Tile

countCombinations :: [Int] -> [Tile] -> Integer
countCombinations = memDoRec 0
 where
  memDoRec = memoize3 doRec

  doRec c (g : _) _
    | c > g = 0
  doRec c (curGoals : goals) (Split : tiles)
    | c == 0 = memDoRec 0 (curGoals : goals) tiles
    | c == curGoals = memDoRec 0 goals tiles
    | otherwise = 0
  doRec c goals (Stuck i : tiles) =
    memDoRec (c + i) goals tiles
  doRec (id -> c) (curGoal : goals) (Un : tiles)
    | c == 0 = useStuck + useSplit
    | c == curGoal = memDoRec 0 goals tiles
    | c < curGoal = useStuck
   where
    useStuck = memDoRec (c + 1) (curGoal : goals) tiles
    useSplit = memDoRec c (curGoal : goals) tiles
  doRec 0 [] tiles | all (\x -> x == Un || x == Split) tiles = 1
  doRec c [g] [] | c == g = 1
  doRec _ _ _ = 0

solveA :: Int -> (Int -> [([Tile], [Int])]) -> Integer
solveA repl inp = sumOn' (\(tiles, goals) -> countCombinations goals tiles) $ inp repl

run :: String -> IO ()
run input = void $ do
  let parsed = parse input
  let resA = solveA 1 parsed
  print resA
  resA @=? 8270

-- let resB = solveA 5 parsed
-- print resB
-- resB @=? 204640299929836
