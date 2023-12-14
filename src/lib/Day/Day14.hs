module Day.Day14 (run) where

import Control.Arrow ((>>>))
import Control.Lens (FunctorWithIndex (imap))
import Data.List (sortOn, transpose)
import Data.List.Extra (groupOn)
import Data.Map qualified as Map
import Data.Ord (Down (Down))
import Test.HUnit ((@=?))

data Tile = Rolling | Empty | Stuck deriving (Eq, Ord)

parse :: String -> [[Tile]]
parse = lines >>> map (map toTile)
 where
  toTile '.' = Empty
  toTile '#' = Stuck
  toTile 'O' = Rolling

type Grid = [[Tile]]

roll :: (Ord b) => (Grid -> Grid) -> (Tile -> b) -> Grid -> Grid
roll transposer sortDirection =
  transposer
    >>> fmap
      ( groupOn (== Stuck)
          >>> fmap (sortOn sortDirection)
          >>> concat
      )
    >>> transposer

sumRolling :: Grid -> Int
sumRolling =
  reverse
    >>> imap (\i -> (succ i *) . length . filter (== Rolling))
    >>> sum

rollNorth, rollWest, rollEast, rollSouth :: Grid -> Grid
rollNorth = roll transpose id
rollWest = roll id id
rollSouth = roll transpose Down
rollEast = roll id Down

oneCycle :: Grid -> Grid
oneCycle = rollNorth >>> rollWest >>> rollSouth >>> rollEast

findEnd :: Integer -> Integer -> Map.Map Grid Integer -> Grid -> Grid
findEnd goal i store xs = case Map.lookup xs store of
  Just prev
    | let newI = prev - 1 + rem goal (i - prev) ->
        fst $ Map.findMin $ Map.filter (== newI) store
  Nothing -> findEnd goal (i + 1) (Map.insert xs i store) (oneCycle xs)

solveA :: Grid -> Int
solveA = rollNorth >>> sumRolling

solveB :: [[Tile]] -> Int
solveB = findEnd (1000000000) 0 mempty >>> sumRolling

run :: String -> IO ()
run input = do
  let parsed = parse input
  let resA = solveA parsed
  print resA
  resA @=? 108857
  let resB = solveB parsed
  print resB
  resB @=? 95273