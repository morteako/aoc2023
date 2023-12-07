module Day.Day07 (run) where

import Control.Arrow
import Control.Lens
import Data.Char (digitToInt, isDigit)
import Data.Coerce (coerce)
import Data.List (find, partition, sort, sortOn)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Ord (Down (Down))
import Data.Tuple (swap)
import Test.HUnit ((@=?))

data Card = Num Int | T | J | Q | K | A deriving (Show, Read, Eq, Ord)

parse :: String -> [([Card], Int)]
parse = lines >>> map parseLine
 where
  parseLine (words -> [cards, bid]) = (map parseCard cards, read bid)
  parseCard x | isDigit x = Num $ fromIntegral $ digitToInt x
  parseCard x = read [x]

newtype JokerCard = JokerCard Card
  deriving (Eq)

instance Ord JokerCard where
  JokerCard J <= _ = True
  _ <= JokerCard J = False
  JokerCard a <= JokerCard b = a <= b

twoFirst (x : y : _) = (fst x, fst y)
twoFirst (x : _) = (fst x, 0)

cardListToMap :: (Ord k) => [k] -> Map k Int
cardListToMap = Map.unionsWith (+) . map (flip Map.singleton 1)

solveA :: [([Card], Int)] -> Int
solveA = sumOfRanks id id

solveB :: [([Card], Int)] -> Int
solveB = sumOfRanks coerce useJ

sumOfRanks :: (Ord b) => ([Card] -> [b]) -> ([(Int, b)] -> [(Int, b)]) -> [([Card], Int)] -> Int
sumOfRanks mapCard swapJ = fmap (first f) >>> sortOn fst >>> imap (\(succ -> i) (snd -> bet) -> i * bet) >>> sum
 where
  f (mapCard -> card) = (getCardValues card, card)
  getCardValues = cardListToMap >>> Map.toList >>> map swap >>> sortOn Down >>> swapJ >>> twoFirst

useJ :: [(Int, JokerCard)] -> [(Int, JokerCard)]
useJ cards = case partition ((== JokerCard J) . snd) cards of
  (j, []) -> j
  ([(jcount, JokerCard J)], rest) -> over (_head . _1) (+ jcount) rest
  ([], rest) -> rest
  _ -> error "nop"

run :: String -> IO ()
run input = do
  let parsed = parse input
  let resA = solveA parsed
  print resA
  resA @=? 253313241
  let resB = solveB parsed
  print resB
  resB @=? 253362743
