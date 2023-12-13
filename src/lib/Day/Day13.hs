module Day.Day13 (run) where

import Control.Arrow ((>>>))
import Control.Lens (imap)
import Data.List
import Data.List.Extra (splitOn)
import Data.Maybe (fromJust)
import Test.HUnit ((@=?))
import Utils

parse :: String -> [[String]]
parse = splitOn "\n\n" >>> fmap (lines)

splits :: (Int -> RowOrCol) -> [a] -> [(([a], [a]), RowOrCol)]
splits tag xs = imap (\i -> (,tag $ succ i)) $ init $ zip (tail $ reverse $ tails $ reverse xs) (fmap tail $ tails xs)

data RowOrCol = Row !Int | Col !Int deriving (Show)

bothSplits :: [String] -> [(([String], [String]), RowOrCol)]
bothSplits xs = splits Row xs ++ splits Col (transpose xs)

countEqsOuter :: [String] -> [String] -> Int
countEqsOuter xs ys = sum $ sum <$> zipWith (zipWith (\x y -> if x /= y then 1 else 0)) xs ys

getRowOrCol :: Int -> [String] -> RowOrCol
getRowOrCol smudges = bothSplits >>> find ((== smudges) . uncurry countEqsOuter . fst) >>> fromJust >>> snd

sumAfterReflectionPoint :: Int -> [[String]] -> Int
sumAfterReflectionPoint smugdes = fmap (getRowOrCol smugdes >>> multM) >>> sum
 where
  multM (Row i) = i * 100
  multM (Col i) = i

run :: String -> IO ()
run input = do
  let parsed = parse input

  let resA = sumAfterReflectionPoint 0 parsed
  print resA
  resA @=? 35210

  let resB = sumAfterReflectionPoint 1 parsed
  print resB
  resB @=? 31974
