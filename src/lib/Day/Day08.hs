module Day.Day08 (run) where

import Control.Arrow
import Data.Char (isUpper)
import Data.List.Extra (findIndex, splitOn)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Set qualified as Set
import Test.HUnit ((@=?))

data Dir = L | R deriving (Show, Read, Eq, Ord)

parse :: String -> ([Dir], Map ([Char], Dir) [Char], ([[Char]], [[Char]]))
parse = lines >>> parseDirsAndSteps >>> (\(dirs, Map.fromList -> steps) -> (dirs, steps, getSpecials steps))
 where
  parseDirsAndSteps (dirs : "" : steps) = (map (read @Dir . pure) dirs, concatMap parseStep steps)
  parseStep (splitOn " = " -> [from, to]) = (parseTargets from to)
  parseTargets fr = filter (flip notElem ("()" :: String)) >>> splitOn ", " >>> makeTups fr
  makeTups fr [l, r] = [((fr, L), l), ((fr, R), r)]

  getSpecials = Map.keysSet >>> Set.map fst >>> (getMMM 'A' &&& getMMM 'Z')

  getMMM c = Set.filter (\x -> last x == c) >>> Set.toList

countStepsToFromAAAtoZZZ :: ([Dir], Map (String, Dir) String, c) -> Int
countStepsToFromAAAtoZZZ (cycle -> dirs, dict, _) = fromJust $ findIndex (== "ZZZ") (scanl (getNextNode dict) "AAA" dirs)

getNextNode :: Map (String, Dir) String -> String -> Dir -> String
getNextNode m cur dir = m Map.! (cur, dir)

countsStepsToFromAllStartsToEnd :: ([Dir], Map (String, Dir) String, ([String], [String])) -> Integer
countsStepsToFromAllStartsToEnd (cycle -> dirs, dict, (starts, Set.fromList -> ends)) = getLcm $ map findOne starts
 where
  getLcm = foldr1 lcm . map toInteger
  isEnd x = Set.member x ends
  findOne start = fromJust $ findIndex isEnd (scanl (getNextNode dict) start dirs)

run :: String -> IO ()
run input = do
  let parsed = parse input
  let resA = countStepsToFromAAAtoZZZ parsed
  print resA
  resA @=? 16531
  let resB = countsStepsToFromAllStartsToEnd parsed
  print resB
  resB @=? 24035773251517
