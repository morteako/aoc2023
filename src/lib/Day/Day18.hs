module Day.Day18 (run) where

import Control.Arrow ((>>>))
import Control.Lens
import Data.Char (digitToInt)
import Linear hiding (E)
import Numeric (readHex)
import Test.HUnit ((@=?))
import Utils

data Dir = U | D | L | R deriving (Show, Eq, Ord, Enum, Bounded, Read)

dirToVec :: Dir -> V2 Int
dirToVec x = case x of
  U -> V2 0 (-1)
  D -> V2 0 1
  R -> V2 1 0
  L -> V2 (-1) 0

parse :: String -> [((Dir, Int), (Dir, Int))]
parse = lines >>> map (words >>> f)
 where
  f [d, i, hash] = ((read @Dir d, readInt i), decode hash)

corners :: [(Dir, Int)] -> [V2 Int]
corners = scanl (+) 0 . map (\(dir, i) -> dirToVec dir * fromIntegral i)

decode :: (Eq b, Num b) => [Char] -> (Dir, b)
decode (init -> tail -> '#' : s) = (dir, meters)
 where
  meters = fst $ head $ readHex (take 5 s)
  dir = case digitToInt $ last s of
    0 -> R
    1 -> D
    2 -> L
    3 -> U

solveA :: [(Dir, Int)] -> Int
solveA cmds = shoelaceWithCircum cornersLen path
 where
  cornersLen = sum $ map (\(dir, i) -> i) cmds
  path = corners cmds

shoelaceWithCircum :: Int -> [V2 Int] -> Int
shoelaceWithCircum circumference w = div (fromIntegral (down - up)) 2 + div circumference 2 + 1
 where
  down = sum $ zipWith (*) xs (tail ys)
  up = sum $ zipWith (*) (tail xs) ys
  xs = map (view _x) w
  ys = map (view _y) w

run :: String -> IO ()
run input = do
  let parsed = parse input
  let resA = solveA (map fst parsed)
  print resA
  resA @=? 58550
  let resB = solveA (map snd parsed)
  print resB
  resB @=? 47452118468566
