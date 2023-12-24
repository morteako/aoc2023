module Day.Day22 (run) where

import Control.Arrow ((>>>))
import Control.Lens hiding (un)
import Control.Monad (void)
import Control.Monad.State
import Data.Either (partitionEithers)
import Data.Foldable
import Data.List
import Data.List.Extra (groupOn, splitOn)
import Data.Map (Map, (!))
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Tuple.Extra (uncurry3)

import Linear
import Test.HUnit ((@=?))
import Text.RawString.QQ (r)

parse :: String -> [(Char, (V3 Int, V3 Int))]
parse = lines >>> map parseLine >>> zip ['A' ..]
 where
  parseLine = splitOn "~" >>> map (splitOn "," >>> map read >>> toTriple >>> uncurry3 V3) >>> toTup

  toTriple [a, b, c] = (a, b, c)
  toTup [a, b] = (a, b)

points (a@(V3 x y z), b@(V3 xx yy zz))
  | x /= xx = map (\nx -> set _x nx a) [x .. xx]
  | y /= yy = map (\nx -> set _y nx a) [y .. yy]
  | z /= zz = map (\nx -> set _z nx a) [z .. zz]
  | a == b = [a]
  | otherwise = error $ show (a, b)

type Grid = Map (V3 Int) Char

getMoved labeledIntervals = (moveAll grid, labs)
 where
  labeledPoints = over (each . _2) points labeledIntervals

  grid = Map.fromListWith undefined $ concatMap (\(lab, ps) -> map (,lab) ps) labeledPoints

  labs = Set.fromList $ map fst labeledIntervals

solveA (getMoved -> (moved, labs)) = do
  let cantBeMoved = Set.unions $ filter (\x -> Set.size x <= 1) $ map (flip checkRemove moved) $ Set.toList labs
  Set.size labs - Set.size cantBeMoved

solveB (getMoved -> (moved, labs)) = do
  let deps = Map.filter (not . Set.null) $ Map.fromList $ map (\l -> (l, checkRemove l moved)) $ Set.toList labs
  sum $ map length $ map (\l -> flip evalState deps (getAlls l)) (toList labs)

getAlls :: Char -> _
getAlls l = do
  paths <- get
  let news = Map.map (Set.delete l) $ Map.delete l paths
  let (Map.keys -> ls, news') = Map.partition (Set.null) news
  put news'
  q <- concat <$> traverse getAlls ls
  pure $ ls ++ q

checkRemove lab grid =
  Set.filter (/= lab) $
    Map.foldMapWithKey (\k _ -> f k) $
      Map.filter (== lab) grid
 where
  f n = case grid Map.!? moveDown n of
    Nothing -> mempty
    Just l -> Set.singleton l

moveDown :: V3 Int -> V3 Int
moveDown = over _z pred

moveLabDown :: Char -> Grid -> _
moveLabDown (lab :: Char) grid
  | any ((< 1) . view _z) $ Map.keys movedPoints = grid
  | or (Map.intersectionWith (/=) movedPoints grid) = grid
  | otherwise = moveLabDown lab (movedPoints <> Map.difference grid ls) -- optimize??
 where
  movedPoints = Map.mapKeys moveDown $ ls
  ls = Map.filter (== lab) grid

moveAll grid = foldl' (\g l -> moveLabDown l g) grid ps
 where
  ps = map head . group . map snd $ sortOn (\(V3 x y z, _) -> (z, y, x)) $ Map.toList grid

run :: String -> IO ()
run input = void $ do
  let parsed = parse input

  let resA = solveA parsed
  print resA
  resA @=? 517

  let resB = solveB parsed
  print resB
  resB @=? 61276
