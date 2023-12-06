module Day.Day06 (run) where

import Control.Arrow ((>>>))
import Data.Monoid (Product (Product))
import Test.HUnit ((@=?))

data TimeDist = TimeDist Int Int deriving (Show)

toTuple [x, y] = (x, y)

parse :: String -> [TimeDist]
parse = lines >>> fmap (words >>> tail >>> fmap read) >>> toTuple >>> uncurry (zipWith TimeDist)

productOfNumberOfWaysToBeatRecord :: [TimeDist] -> Product Integer
productOfNumberOfWaysToBeatRecord = foldMap (getRels >>> Product)
 where
  getRels (TimeDist t m) = case solveQuadraticEq (-1) (fromIntegral t) (negate $ fromIntegral m) of
    (succ -> floor -> left, ceiling -> right) -> right - left

solveQuadraticEq :: (Floating a) => a -> a -> a -> (a, a)
solveQuadraticEq a b c = (solve (+), solve (-))
 where
  solve (+-) = ((-b) +- (sqrt (b ^ 2 - (4 * a * c)))) / (2 * a)

combine :: [TimeDist] -> TimeDist
combine ts = TimeDist time dist
 where
  time = read $ concatMap (\(TimeDist t _) -> show t) ts
  dist = read $ concatMap (\(TimeDist _ d) -> show d) ts

combineNumbersThenGetProduct :: [TimeDist] -> Product Integer
combineNumbersThenGetProduct = combine >>> pure >>> productOfNumberOfWaysToBeatRecord

run :: String -> IO ()
run input = do
  let parsed = parse input
  let resA = productOfNumberOfWaysToBeatRecord parsed
  resA @=? 393120
  print resA
  let resB = combineNumbersThenGetProduct parsed
  print resB
  resB @=? 36872656
