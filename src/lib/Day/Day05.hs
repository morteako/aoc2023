{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE NoFieldSelectors #-}

module Day.Day05 (run) where

import Control.Arrow
import Control.Monad (void)
import Data.Function
import Data.IntSet qualified as Set
import Data.IntervalMap (Interval (..))
import Data.IntervalMap.Lazy qualified as IM
import Data.List
import Data.List qualified as List
import Data.List.Extra (firstJust, minimumOn)
import Data.List.Split (splitOn)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Semigroup
import Debug.Trace
import Safe (readMay)
import Test.HUnit ((@=?))
import Text.RawString.QQ (r)
import Utils (traceLab)
import Prelude hiding (max, min)

data Mapping = Mapping {destStart :: Int, sourceStart, range :: Int}

-- q ::
-- q = (.destStart)

-- 1 => 2,  2 => 3,  3 => 4
--

instance Show Mapping where
  show (Mapping x y z) = show x ++ " <= " ++ show y ++ " : " ++ show z

parse :: [Char] -> ([Int], [[Mapping]])
parse = splitOn "\n\n" >>> map lines >>> parseSeedsAndMaps
 where
  parseSeedsAndMaps ([seeds] : maps) = (map (read @Int) (tail $ words seeds), map parseMappings maps)
  parseMappings (_name : lines) = map toMapping $ map parseMapping $ lines
  parseMapping = words >>> map (readInt @Int)
  toMapping [a, b, c] = Mapping a b c
  toMapping xs = error $ show xs

readInt :: (Read a) => String -> a
readInt x = case readMay x of
  Just q -> q
  Nothing -> error x

data Func = Func {adder :: Int, min :: Int, max :: Int} deriving (Show)

mapToFunc :: Mapping -> Func
mapToFunc m = Func (m.destStart - m.sourceStart) m.sourceStart (m.sourceStart + m.range)

-- unpackFunc :: Func -> Int -> Int
-- -- unpackFunc (Func adder min max) x | error $ show adder = undefined
-- unpackFunc (Func adder min max) x | x >= min && x < max = traceLab ("changed:" ++ show x ++ " ," ++ show adder ++ " =>") $ x + adder
-- unpackFunc (Func adder min max) x = traceLab "unchanged" $ traceShow min $ x
unpackFuncs = go
 where
  go :: [Func] -> (Int -> Int)
  go [] x = x
  go (Func adder min max : fs) x | x >= min && x < max = x + adder
  go (_ : fs) x = unpackFuncs fs x

solveA (seeds, mappings) = minimum $ map maps seeds
 where
  maps = foldr1 (.) $ map unpackFuncs $ reverse $ map (map mapToFunc) mappings

getAllSeeds :: [Int] -> [Int]
getAllSeeds (val : range : rest) = take range [val ..] ++ getAllSeeds rest
getAllSeeds [] = []

solveB (getAllSeeds -> traceLab "seeds" -> id -> seeds, mappings) = firstJust check [0 ..]
 where
  check x | mod x 10000 == 0, traceShow x False = undefined
  check (maps -> x) = if elem x seeds then Just x else Nothing
  maps = foldr1 (.) $ map unpackFuncs $ map (map revmapToFunc) mappings

-- getOutside :: [[Mapping]] -> _
-- getOutside mappings = IM.fromList $ map (,()) $ map (\m -> IntervalCO m.sourceStart (m.sourceStart + m.range)) $ concat mappings

revmapToFunc :: Mapping -> Func
revmapToFunc m = Func (m.sourceStart - m.destStart) m.destStart (m.destStart + m.range)

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
  -- input <- putStrLn "#####    testInput   #####" >> pure testInput
  print input
  let parsed = parse input
  -- print parsed
  -- mapM_ print $ snd parsed
  let resA = solveA parsed
  print "--------------------------------------------"
  print resA

  -- let f = Func{adder = 70, min = 18, max = 25}
  -- print $ "\n\n" ++ (show $ unpackFunc f $ 53)
  -- print $ unpackFunc (mapToFunc $ Mapping 18 25 70) 53

  -- resA @=? 1715
  let resB = solveB parsed
  print resB

-- resB @=? 1739
