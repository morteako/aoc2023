module Day.Day01 (run) where

import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as C
import Data.ByteString.Search qualified as Search
import Data.Char (isDigit)
import Data.Foldable (foldl')
import Test.HUnit ((@=?))

main = interact $ \inp -> do
  let nums = read @[Int] $ "[" ++ inp ++ "]"
  show $ sum $ maximumOn length $ groupBy (<=) nums

parse :: String -> [C.ByteString]
parse = C.lines . C.pack

-- sum of first and last digit for every line
solveA :: [C.ByteString] -> Int
solveA = sum . map getFirstAndLastDigit
 where
  getFirstAndLastDigit (C.filter isDigit -> digits) =
    read [C.head digits, C.last digits]

-- sum of first and last digit, replacing one->1,two->2,.. , for every line
solveB :: [C.ByteString] -> Int
solveB = sum . map (\line -> getFirstAndLastDigit2 (replaceNumbers True line) (replaceNumbers False line))
 where
  -- not the best solution, but it works :))
  getFirstAndLastDigit2 (C.filter isDigit -> digits1) (C.filter isDigit -> digits2) =
    read [C.head digits1, C.last digits2]

replaceNumbers :: Bool -> C.ByteString -> C.ByteString
replaceNumbers isFindFirst s = foldl' f s subs
 where
  f cur (int, str) = BS.toStrict $ Search.replace str (comb str int) cur
  comb str (C.pack . show -> digit) = if isFindFirst then str <> digit else digit <> str

  subs = zip [1 ..] $ map C.pack ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

run :: String -> IO ()
run input = do
  let parsed = parse input
  let resA = solveA parsed
  print resA
  resA @=? 53921

  let resB = solveB parsed
  print resB
  resB @=? 54676
