{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE NoFieldSelectors #-}

module Day.Day05 (run) where

import Control.Arrow
import Control.Lens
import Control.Monad (void)
import Data.Function
import Data.IntMap (IntMap)
import Data.IntMap qualified as Map
import Data.IntSet qualified as Set
import Data.List
import Data.List qualified as List
import Data.List.Extra (firstJust, minimumOn)
import Data.List.Split (splitOn)
import Data.Map (Map)
import Data.Semigroup
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Debug.Trace
import Safe (readMay)
import Test.HUnit ((@=?))
import Text.RawString.QQ (r)
import Utils (traceLab)
import Prelude hiding (max, min)

data Mapping = Mapping {destStart, sourceStart, range :: Int}

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

-- newtype Adder = Adder Int deriving (Show)

-- asdf
mapToFunc :: Mapping -> Func
mapToFunc m = Func (m.destStart - m.sourceStart) m.sourceStart (m.sourceStart + m.range)

type M = IntMap Int

funcToM :: Func -> M
funcToM f = Map.singleton f.min (f.adder)

combMap from to = Map.mapWithKey (\k v -> Map.findWithDefault v k to)

par x = "(" ++ x ++ ")"

pack = Text.pack . show

toZ3 :: Text -> Text -> Func -> Text
toZ3 oldId newId f =
  repl "#MIN" (pack f.min) $
    repl "#MAX" (pack f.max) $
      repl "#NEWID" newId $
        repl "#OLDID" oldId $
          repl "#ADDER" (pack f.adder) t
 where
  t = "(assert (if (and (>= #MIN #OLDID) (< #OLDID #MAX)) (= #NEWID (+ #OLDID #ADDER)) (= #NEWID #OLDID )))"
  repl old new = Text.replace old new

-- unpackFunc :: Func -> Int -> Int
-- -- unpackFunc (Func adder min max) x | error $ show adder = undefined
-- unpackFunc (Func adder min max) x | x >= min && x < max = traceLab ("changed:" ++ show x ++ " ," ++ show adder ++ " =>") $ x + adder
-- unpackFunc (Func adder min max) x = traceLab "unchanged" $ traceShow min $ x
unpackFuncs = go . sortOn (.min)
 where
  go :: [Func] -> (Int -> Int)
  go [] x = x
  go (Func adder min max : fs) x | x >= min && x < max = x + adder
  go (_ : fs) x = unpackFuncs fs x

toId i = Text.pack $ "a" ++ show i

solveA (seeds, mappings) = Text.unlines $ decls ++ [seedDecl] ++ assertMappings
 where
  funcs = map (map mapToFunc) mappings
  assertMappings = foldMap id $ imap (\i m -> map (toZ3 (toId i) (toId (i + 1))) m) $ funcs

  decls = imap (\i _ -> declareConst i) $ (funcs ++ [[]])

  seedDecl = "(assert (or " <> ss <> "))"
  ss = Text.intercalate " " $ map (\x -> "(= a0 " <> pack x <> ")") seeds

declareConst id = "(declare-const " <> (toId id) <> " Int)"

-- maps = foldr1 combMap $ map unpackFuncs $ reverse $ map (map mapToFunc) mappings

-- getAllSeeds :: [Int] -> [Int]
-- getAllSeeds (val : range : rest) = take range [val ..] ++ getAllSeeds rest
-- getAllSeeds [] = []

-- solveB (getAllSeeds -> traceLab "seeds" -> id -> seeds, mappings) = firstJust check [minimum (map (.destStart) (last mappings)) ..]
--  where
--   check x | mod x 10000 == 0, traceShow x False = undefined
--   check (maps -> x) = if elem x seeds then Just x else Nothing
--   maps = foldr1 (.) $ map unpackFuncs $ map (map revmapToFunc) mappings

-- getOutside :: [[Mapping]] -> _
-- getOutside mappings = IM.fromList $ map (,()) $ map (\m -> IntervalCO m.sourceStart (m.sourceStart + m.range)) $ concat mappings

revmapToFunc :: Mapping -> Func
revmapToFunc m = Func (m.sourceStart - m.destStart) m.destStart (m.destStart + m.range)

testInput =
  [r|seeds: 79 14 55 100

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
  mapM_ print $ map (foldMap funcToM . map mapToFunc) $ snd parsed
  let resA = solveA parsed
  print "--------------------------------------------"
  Text.putStrLn resA
  print "--------------------------------------------"

-- let f = Func{adder = 70, min = 18, max = 25}
-- print $ "\n\n" ++ (show $ unpackFunc f $ 53)
-- print $ unpackFunc (mapToFunc $ Mapping 18 25 70) 53

-- -- resA @=? 1715
-- let resB = solveB parsed
-- print resB

-- resB @=? 1739
