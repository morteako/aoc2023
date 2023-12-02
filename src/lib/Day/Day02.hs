module Day.Day02 (run) where

import Control.Arrow ((>>>))
import Control.Lens (imap)
import Data.List.Extra (splitOn, sumOn')
import Data.Map qualified as Map
import Test.HUnit ((@=?))

parse :: String -> [(Int, [Map.Map String Int])]
parse = lines >>> imap parseGame
 where
  parseGame i l = (i + 1, dropWhile (/= ':') >>> tail >>> splitOn "; " >>> map (splitOn ", " >>> foldMap parseNumCol) $ l)
  parseNumCol (words -> [num, col]) = Map.singleton col (read num)

gameBag :: Map.Map String Int
gameBag =
  Map.fromList
    ["red" =: 12, "green" =: 13, "blue" =: 14]
 where
  (=:) = (,)

solveA :: [(Int, [Map.Map String Int])] -> Int
solveA = filter (\(_, rows) -> all inside rows) >>> sumOn' fst
 where
  inside r = all (>= 0) $ Map.unionWith (-) gameBag r

solveB :: [(a, [Map.Map String Int])] -> Int
solveB = map (snd >>> Map.unionsWith max >>> product) >>> sum

run :: String -> IO ()
run input = do
  let parsed = parse input

  let resA = solveA parsed
  print resA
  resA @=? 1867

  let resB = solveB parsed
  print resB
  resB @=? 84538
