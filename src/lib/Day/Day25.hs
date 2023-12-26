module Day.Day25 (run) where

import Control.Arrow ((>>>))
import Control.Lens
import Data.Graph.Inductive (Gr, Graph (labNodes, mkGraph), LPath (unLPath), scc)
import Data.Graph.Inductive.Query
import Data.List
import Data.List.Split
import Data.Set qualified as Set
import Data.Tuple
import Test.HUnit ((@=?))

parse = lines >>> map parseLine >>> concat
 where
  parseLine = splitOn ": " >>> f
  f [name, rest] = fmap (name,) $ words rest

addBoth xs = xs ++ fmap swap xs

makeGraph :: [(String, String)] -> _ -> Gr _ _
makeGraph cons toRem = mkGraph lnodes ledges
 where
  nods = toListOf (each . each) cons
  labs = Set.fromList nods
  toInd x = Set.findIndex x labs

  lnodes = fmap (\x -> (toInd x, x)) nods
  ledges = fmap (\(x, y) -> (toInd x, toInd y, 1)) $ filter (flip notElem toRem) $ cons

solveA :: [(String, String)] -> Int
solveA (addBoth -> cons) = do
  let gr = makeGraph cons []
  let nodes = labNodes gr

  let relevantNodea = fmap snd $ take 6 $ flip sortOn nodes $ \(l, _) ->
        (length $ (unLPath) $ last $ spTree l gr)
  let edgesToRemove = (,) <$> relevantNodea <*> relevantNodea

  product $ map length $ scc $ makeGraph cons (addBoth edgesToRemove)

run :: String -> IO ()
run input = do
  let parsed = parse input
  let resA = solveA parsed
  print resA
  resA @=? 520380
