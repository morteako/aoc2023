module Day.Day15 (run) where

import Control.Arrow ((>>>))
import Control.Monad (void)

import Data.Char (ord)
import Data.Foldable (Foldable (foldl', toList))
import Data.IntMap qualified as Map
import Data.List.Split (splitOn)
import Data.Map.Ordered (OMap)
import Data.Map.Ordered qualified as OMap
import Test.HUnit ((@=?))

data LabeledLens = Dash String | Equals String Int deriving (Show)

parse :: String -> [(String, LabeledLens)]
parse = filter (/= '\n') >>> splitOn "," >>> fmap (\s -> (s, toT s))
 where
  toT t
    | [l, r] <- splitOn "=" t = Equals l (read r)
    | otherwise = Dash $ init t

getVal :: Int -> Char -> Int
getVal curVal (ord -> o) = ((curVal + o) * 17) `rem` 256

hashVal :: String -> Int
hashVal = (foldl' getVal 0)

solveA :: [(String, b)] -> Int
solveA = sum . map hashVal . map fst

insertLens :: Map.IntMap (OMap String Int) -> LabeledLens -> Map.IntMap (OMap String Int)
insertLens m (Dash lab) =
  let hv = hashVal lab
   in Map.adjust (OMap.alter (const Nothing) lab) hv m
insertLens m (Equals lab focal) =
  let hv = hashVal lab
      tup = (lab, focal)
      sing = OMap.singleton tup
   in Map.insertWith (\new old -> tup OMap.<| old) hv sing m

solveB :: [(a, LabeledLens)] -> Int
solveB = sum . Map.foldMapWithKey (\k v -> map (* (k + 1)) v) . indexedOrder . foldl' insertLens Map.empty . map snd
 where
  indexedOrder = fmap (zipWith (*) [1 ..] . reverse . toList)

  calc (i, (a, b)) = i * a * b

run :: String -> IO ()
run input = void $ do
  let parsed = parse input
  let resA = solveA parsed
  print resA
  resA @=? 510273
  let resB = solveB parsed
  print resB
  resB @=? 212449
