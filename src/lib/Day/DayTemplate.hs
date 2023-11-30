module Day.DayTemplate (run) where

import Control.Monad (void)
import Test.HUnit ((@=?))
import Text.RawString.QQ (r)

parse = id

solveA = id

solveB = id

testInput =
  [r|
1000
2000
3000

4000

5000
6000

7000
8000
9000

10000
|]

run :: String -> IO ()
run input = void $ do
  input <- putStrLn "#####    testInput   #####" >> pure testInput
  print input
  let parsed = parse input
  print parsed
  let resA = solveA parsed
  print resA

-- resA @=? 1715
-- let resB = solveB parsed
-- print resB
-- resB @=? 1739
