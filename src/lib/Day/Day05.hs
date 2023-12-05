{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Day.Day05 (run) where

import Control.Arrow
import Control.Monad (void)
import Data.List qualified as List
import Data.List.Split (splitOn)
import Data.Map (Map)
import Data.Map qualified as Map
import Debug.Trace
import Safe (readMay)
import Test.HUnit ((@=?))
import Text.RawString.QQ (r)
import Utils (traceLab)
import Prelude hiding (max, min)

data Mapping = Mapping {typeFrom :: String, typeTo :: String, destStart :: Int, sourceStart, range :: Int}

-- q ::
-- q = (.destStart)

-- 1 => 2,  2 => 3,  3 => 4
--

instance Show Mapping where
  show (Mapping from to x y z) = from ++ "-" ++ to ++ "   " ++ show x ++ " <= " ++ show y ++ " : " ++ show z

parse = splitOn "\n\n" >>> map lines >>> parseSeedsAndMaps
 where
  parseSeedsAndMaps ([seeds] : maps) = (map ("seeds",) $ map (read @Int) (tail $ words seeds), map parseMappings maps)
  parseMappings (name : lines) = map (toMapping (parseName name) . parseMapping) $ lines
  parseName (words -> head -> splitOn "-" -> [from, "to", to]) = (from, to)

  parseMapping = words >>> map (read @Int)

  toMapping (from, to) [a, b, c] = Mapping from to a b c

-- toMapping xs = error $ show xs

data Func = Func {typeFrom :: String, typeTo :: String, adder :: Int, min :: Int, max :: Int} deriving (Show)

mapToFunc :: Mapping -> Func
mapToFunc m = Func m.typeFrom m.typeTo (m.destStart - m.sourceStart) m.sourceStart (m.sourceStart + m.range)

unpackFunc :: Func -> (String, Int) -> (String, Int)
unpackFunc (Func tfrom tto adder min max) (typ, x)
  | x >= min && x < max && tfrom == typ = (tto, x + adder)
unpackFunc _ x = x

solveA (seeds, mappings) = map maps seeds
 where
  maps = foldr1 (.) $ map unpackFunc $ reverse $ concatMap (map mapToFunc) mappings

solveB :: a -> a
solveB = id

testInput =
  [r|seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4
|]

run :: String -> IO ()
run input = void $ do
  input <- putStrLn "#####    testInput   #####" >> pure testInput
  print input
  let parsed = parse input
  print parsed
  let resA = solveA parsed
  print resA

-- let f = Func{adder = 70, min = 18, max = 25}
-- print $ "\n\n" ++ (show $ unpackFunc f $ 53)
-- print $ unpackFunc (mapToFunc $ Mapping 18 25 70) 53

-- resA @=? 1715
-- let resB = solveB parsed
-- print resB
-- resB @=? 1739
