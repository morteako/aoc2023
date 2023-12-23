module Day.Day22 (run) where

import Control.Arrow ((>>>))
import Control.Lens hiding (un)
import Control.Monad (void)
import Data.Either (partitionEithers)
import Data.Foldable
import Data.List
import Data.List.Extra (groupOn, splitOn)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Monoid
import Data.Set qualified as Set
import Data.Tuple.Extra (uncurry3)
import Data.Vector.Generic.Mutable (move)
import Debug.Trace
import Linear
import Test.HUnit ((@=?))
import Text.RawString.QQ (r)
import Utils hiding (printV2Map)

parse :: String -> [(Char, (V3 Int, V3 Int))]
parse = lines >>> map parseLine >>> zip ['A' ..]
 where
  parseLine = splitOn "~" >>> map (splitOn "," >>> map readInt >>> toTriple >>> uncurry3 V3) >>> toTup

  toTriple [a, b, c] = (a, b, c)
  toTup [a, b] = (a, b)

points (a@(V3 x y z), b@(V3 xx yy zz))
  | x /= xx = map (\nx -> set _x nx a) [x .. xx]
  | y /= yy = map (\nx -> set _y nx a) [y .. yy]
  | z /= zz = map (\nx -> set _z nx a) [z .. zz]
  | a == b = [a]
  | otherwise = error $ show (a, b)

type Grid = Map (V3 Int) Char

pattern Dot = '.'

emptyGrid6 = Map.fromList $ map (,Dot) $ V3 <$> [0 .. 2] <*> [0 .. 2] <*> [1 .. 6]
emptyGrid9 = Map.fromList $ map (,Dot) $ V3 <$> [0 .. 2] <*> [0 .. 2] <*> [1 .. 9]

toY :: Map (V3 Int) Char -> Map (V2 Int) Char
toY = Map.mapKeysWith comb (\(V3 x y z) -> V2 y z)

toX :: Map (V3 Int) Char -> Map (V2 Int) Char
toX = Map.mapKeysWith comb (\(V3 x y z) -> V2 x z)

comb x y | x == y = x
comb Dot y = y
comb x Dot = x
comb _x _y = '?'

solveA labeledIntervals = do
  mprint labeledPoints
  mprint grid
  mprint labs
  -- mprint $ map (flip moveLabDown grid) $ Set.toList labs

  -- mprint $ sortOn (view _z . fst) $ Map.toList grid
  -- print "---------X"
  -- printV2Map toX moved
  -- print "---------Y"
  -- printV2Map toY moved
  -- print "MOVED"
  -- mprint $ sortOn (negate . view _z . fst) $ Map.toList $ moved

  -- print $ grid == moveAll grid -- False, men FOR LIKT?
  -- printlab "labsizes" labSizes
  let cantBeMoved = Set.unions $ filter (\x -> Set.size x <= 1) $ map (flip checkRemove moved) $ Set.toList labs
  print $ Set.size labs - Set.size cantBeMoved
 where
  -- printlab "moop" $ checkRemove 'c' moved

  -- q = _
  -- comb = Map.unionWith q (emptyGrid 9)
  moved = moveAll grid
  labeledPoints = over (each . _2) points labeledIntervals

  grid = Map.fromListWith undefined $ concatMap (\(lab, ps) -> map (,lab) ps) labeledPoints

  labs = Set.fromList $ map fst labeledIntervals

  labSizes = ifoldMap (\k v -> Set.singleton (k, v)) $ Map.fromListWith (+) $ foldMap (\v -> [(v, 1 :: Int)]) grid

checkRemove lab grid =
  Set.filter (/= lab) $
    Map.foldMapWithKey (\k _ -> f k) $
      Map.filter (== lab) grid
 where
  -- check l c = Set.member (l, c) labSizes

  f n = case grid Map.!? (over _z pred n) of
    Nothing -> mempty
    Just l -> Set.singleton l

moveDown = over _z pred

un = undefined

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

-- printV2Map :: (Show a, Ord k, Num k) => Map.Map (V2 k) a -> IO ()
printV2Map f (flip Map.union emptyGrid6 -> f -> m) = do
  putStrLn "--------"
  let xs = Map.toList m
  let g = groupOn (\(V2 _ y, _) -> y) $ sortOn (\(V2 x y, _) -> V2 (-y) x) xs
  let gg = fmap (fmap snd) g
  mapM_
    ( \x -> do
        mapM_ (putChar) x
        putStrLn ""
    )
    $ gg
  putStrLn ""

solveB = id

testInput =
  [r|1,0,1~1,2,1
0,0,2~2,0,2
0,2,3~2,2,3
0,0,4~0,2,4
2,0,5~2,2,5
0,1,6~2,1,6
1,1,8~1,1,9
|]

run :: String -> IO ()
run input = void $ do
  -- input <- putStrLn "#####    testInput   #####" >> pure testInput
  print input
  let parsed = parse input
  mprint parsed
  let resA = solveA parsed
  resA

-- resA @=? 1715
-- let resB = solveB parsed
-- print resB
-- resB @=? 1739
