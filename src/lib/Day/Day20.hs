module Day.Day20 (run) where

import Control.Arrow ((>>>))
import Control.Lens
import Control.Monad (guard, replicateM, void)
import Control.Monad.State
import Data.Either (partitionEithers)
import Data.Foldable
import Data.List.Extra hiding ((!?))
import Data.Map (Map, (!), (!?))
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Debug.Trace (traceShow, traceShowM)
import Test.HUnit ((@=?))
import Text.RawString.QQ (r)
import Utils

data M = Mod String | Con String

instance Show M where
  show (Mod s) = "%" ++ s
  show (Con s) = "&" ++ s

-- show Broad = "broadcaster"

data Stuff = Stuff {starts :: [Signal], m :: Map String (Switch, [String])} deriving (Show)

parse = lines >>> map parseLine >>> partitionEithers >>> makeStuff
 where
  parseLine (splitOn " -> " -> [l, r]) = ((toM l) $ splitOn ", " r)

  toM ('%' : rest) t = Right (rest, (Flip Off, t))
  toM ('&' : rest) t = Right (rest, (Conj mempty, t))
  toM "broadcaster" t = Left $ (t)

  makeStuff ([t], ms) = (Stuff (map (Signal "BC" Low) t) $ makeMap (Map.fromList ms) (traceLax "fm" $ (Map.unionsWith (<>) $ map f ms)))

  makeMap xs revMap = Map.mapWithKey (g revMap) xs

  g m k (Conj _, xs) = (Conj (m ! k), xs)
  g m k xs = xs

  f (k, (_, xs)) = traceLax "f" $ Map.fromListWith (<>) $ map (,Set.fromList [k]) xs

data Signal = Signal String Bool String

instance Show Signal where
  show (Signal from Low to) = from ++ "|-low-> " ++ to ++ "! "
  show (Signal from High to) = from ++ "|-high-> " ++ to ++ "! "

pattern Low = False
pattern High = True

data Sta = On | Off deriving (Show)

toggle On = Off
toggle Off = On

toBool On = True
toBool Off = False

addC (!lows, !highs) Low = (lows + 1, highs)
addC (!lows, !highs) High = (lows, highs + 1)

addToState b = modify (over _2 (flip addC b))

type Counter = (Int, Int)

traceLax s a = a

-- sendPuls :: [Signal] -> State (Map String (Switch, [String]), Counter) ()
sendPuls (traceLax "sigs" -> []) = pure ()
sendPuls (Signal from b to : curs) = do
  addToState b
  m <- gets fst
  case m !? to of
    Nothing -> sendPuls curs
    Just x -> case x of
      (Flip _, _) | b == High -> sendPuls curs
      (Flip (toggle -> fs), nexts) -> do
        modifying _1 (Map.insert to (Flip fs, nexts))
        sendPuls (curs ++ fmap (Signal to $ toBool fs) nexts)
      (Conj lows, nexts) -> do
        -- _ <- traceShow (b,lows) $ pure ()
        let lows' = case b of
              High -> Set.delete from lows
              Low -> Set.insert from lows
        modifying _1 (Map.insert to (Conj lows', nexts))
        let newSig = not $ Set.null lows'
        -- _ <- traceShow ("newSIg", newSig, from, to, b, lows, lows') $ pure ()
        sendPuls (curs ++ fmap (Signal to newSig) nexts)

-- Flip-flop modules (prefix %) are either on or off; they are initially off. If a flip-flop module receives a high pulse, it is ignored and nothing happens. However, if a flip-flop module receives a low pulse, it flips between on and off. If it was off, it turns on and sends a high pulse. If it was on, it turns off and sends a low pulse.

-- Conjunction modules (prefix &) remember the type of the most recent pulse received from each of their connected input modules; they initially default to remembering a low pulse for each input. When a pulse is received, the conjunction module first updates its memory for that input. Then, if it remembers high pulses for all inputs, it sends a low pulse; otherwise, it sends a high pulse.

data Switch = Flip !Sta | Conj !(Set String) deriving (Show)

solveA (Stuff starts m) = productOf each $ snd $ snd $ flip runState (m, (0, 0)) $ replicateM 1000 (addToState Low >> sendPuls starts)

solveB = id

testInput =
  (!! 1) $
    [ [r|broadcaster -> a, b, c
%a -> b
%b -> c
%c -> inv
&inv -> a
|]
    , [r|broadcaster -> a
%a -> inv, con
&inv -> b
%b -> con
&con -> output|]
    ]

-- The module configuration (your puzzle input) lists each module. The name of the module is preceded by a symbol identifying its type, if any. The name is then followed by an arrow and a list of its destination modules. For example:

run :: String -> IO ()
run input = void $ do
  -- input <- putStrLn "#####    testInput   #####" >> pure testInput
  print input
  let parsed = parse input
  mprint parsed
  print "--------"

  let resA = solveA parsed
  mprint resA

-- resA @=? 1715
-- let resB = solveB parsed
-- print resB
-- resB @=? 1739
