{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE NoFieldSelectors #-}

module Day.Day05 (run) where

import Control.Arrow
import Control.Lens
import Control.Monad (void)
import Data.Containers.ListUtils (nubOrd)
import Data.Function
import Data.IntMap (IntMap)
import Data.IntMap qualified as Map
import Data.IntSet qualified as IntSet
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

data Mapping = Mapping {destStart, sourceStart, range :: Integer}

-- q ::
-- q = (.destStart)

-- 1 => 2,  2 => 3,  3 => 4
--

instance Show Mapping where
  show (Mapping x y z) = show x ++ " <= " ++ show y ++ " : " ++ show z

parse :: [Char] -> ([Integer], [[Mapping]])
parse = splitOn "\n\n" >>> map lines >>> parseSeedsAndMaps
 where
  parseSeedsAndMaps ([seeds] : maps) = (map (read @Integer) (tail $ words seeds), map parseMappings maps)
  parseMappings (_name : lines) = map toMapping $ map parseMapping $ lines
  parseMapping = words >>> map (readInt @Integer)
  toMapping [a, b, c] = Mapping a b c
  toMapping xs = error $ show xs

readInt :: (Read a) => String -> a
readInt x = case readMay x of
  Just q -> q
  Nothing -> error x

data Func = Func {adder :: Integer, min :: Integer, max :: Integer} deriving ()

instance Show Func where
  show (Func adder mi ma) =
    concat
      [ "{"
      , s mi
      , "-"
      , s ma
      , " ["
      , if adder >= 0 then "+" else ""
      , s adder
      , "] "
      , s $ adder + mi
      , "-"
      , s $ adder + ma
      , "}"
      ]
   where
    s = show

-- newtype Adder = Adder Int deriving (Show)

-- asdf
mapToFunc :: Mapping -> Func
mapToFunc m = Func (m.destStart - m.sourceStart) m.sourceStart (m.sourceStart + m.range)

-- funcToM :: Func -> M
-- funcToM f = Map.singleton f.min (f.adder)

-- combMap from to = Map.mapWithKey (\k v -> Map.findWithDefault v k to)

-- unpackFunc :: Func -> Int -> Int
-- -- unpackFunc (Func adder min max) x | error $ show adder = undefined
-- unpackFunc (Func adder min max) x | x >= min && x < max = traceLab ("changed:" ++ show x ++ " ," ++ show adder ++ " =>") $ x + adder
-- unpackFunc (Func adder min max) x = traceLab "unchanged" $ traceShow min $ x
unpackFuncs = go . sortOn (.min)
 where
  go :: [Func] -> (Integer -> Integer)
  go [] x = x
  go (Func adder min max : fs) x | x >= min && x < max = x + adder
  go (_ : fs) x = unpackFuncs fs x

toId i = Text.pack $ "a" ++ show i

solveA (seeds, mappings) = "hei"

-- maps = foldr1 combMap $ map unpackFuncs $ reverse $ map (map mapToFunc) mappings

getSeedRanges :: [Integer] -> [(Integer, Integer)]
getSeedRanges (val : range : rest) = (val, val + range) : getSeedRanges rest
getSeedRanges [] = []

-- PRØVE å dunke ranges og så holde tritt på min?

solveB (seeds, mappings) = head wtf -- zipWith map maps (traceLab "limsMi" limsMi)
 where
  wtf = sort $ map normal $ keepSeeds $ concatMap f $ concat $ (zipWith map maps limsMi ++ zipWith map maps limsMa)
  allSeeds = sort $ getSeedRanges seeds

  f x = [x, x - 1, x + 1]
  -- fil = flip checkInside allSeeds
  keepSeeds = filter (\i -> any (\(mi, ma) -> i >= mi && i <= ma) allSeeds)
  -- check x | mod x 10000 == 0, traceShow x False = undefined
  -- check (maps -> x) = if IntSet.member x allSeeds then Just x else Nothing
  maps = scanl1 (.) $ map unpackFuncs $ map (map revmapToFunc) mappings

  normal = foldr1 (.) $ reverse $ map unpackFuncs $ map (map mapToFunc) mappings

  -- relSeeds = minses

  -- (minses, miadds) = unzip $ map (\m -> (m.min, m.min + m.adder)) $ map mapToFunc $ concat mappings

  limsMi = map (map (.min) . map mapToFunc) mappings
  limsMa = map (map (.max) . map mapToFunc) mappings

-- xs = nubOrd $ sort $ (+) <$> minses <*> miadds

revmapToFunc :: Mapping -> Func
revmapToFunc m = Func (m.sourceStart - m.destStart) m.destStart (m.destStart + m.range)

testInput =
  [r|seeds: 50 10

seed-to-soil map:
40 55 56

seed-to-soil map:
30 42 10
|]

run :: String -> IO ()
run input = void $ do
  -- input <- putStrLn "#####    testInput   #####" >> pure testInput
  print $ 123
  print input
  let parsed = parse input
  -- print parsed
  mapM_ print $ map (map mapToFunc) $ snd parsed
  let resA = solveA parsed
  print "--------------------------------------------"
  Text.putStrLn resA
  print "--------------------------------------------"

  -- let f = Func{adder = 70, min = 18, max = 25}
  -- print $ "\n\n" ++ (show $ unpackFunc f $ 53)
  -- print $ unpackFunc (mapToFunc $ Mapping 18 25 70) 53

  -- -- resA @=? 1715
  let resB = solveB parsed
  print resB

-- resB @=? 1739
