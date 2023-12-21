module Day.Day20 (run) where

import Control.Arrow ((>>>))
import Control.Lens
import Control.Monad (guard, replicateM, void)
import Control.Monad.State
import Data.Either (partitionEithers)
import Data.Foldable
import Data.List
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

data Sta = On | Off deriving (Show, Eq)

toggle On = Off
toggle Off = On

toBool On = True
toBool Off = False

addC (!lows, !highs) Low = (lows + 1, highs)
addC (!lows, !highs) High = (lows, highs + 1)

addToState b = modify (over _2 (flip addC b))

type Counter = (Int, Int)

traceLax s a = a

-- TESTE ALL ON?

-- sendPuls :: [Signal] -> State (Map String (Switch, [String]), Counter) ()
sendPuls (traceLax "sigs" -> []) = pure False
sendPuls (Signal from s "rx" : curs) = error $ show s
-- sendPuls (Signal from s "xt" : curs) = traceShow ("\n\n\n", s, from) $ pure $ True
sendPuls (Signal "xt" s t : curs) = error t
-- sendPuls (Signal from s t : curs) | elem t targets || elem from targets = do
--   others <- sendPuls curs
--   pure (from : others)
sendPuls (Signal from b to : curs) = do
  m <- get
  case m !? to of
    Nothing -> sendPuls curs
    Just x -> case x of
      (Flip _, _) | b == High -> sendPuls curs
      (Flip (toggle -> fs), nexts) -> do
        modify (Map.insert to (Flip fs, nexts))
        sendPuls (curs ++ fmap (Signal to $ toBool fs) nexts)
      (Conj lows, nexts) -> do
        -- _ <- traceShow (b,lows) $ pure ()
        let lows' = case b of
              High -> Set.delete from lows
              Low -> Set.insert from lows
        modify (Map.insert to (Conj lows', nexts))
        let newSig = not $ Set.null lows'
        -- _ <- traceShow ("newSIg", newSig, from, to, b, lows, lows') $ pure ()
        sendPuls (curs ++ fmap (Signal to newSig) nexts)

isConj (Conj _) = True
isConj (Flip _) = False

targets = ["lk" :: String, "zv", "xt", "sp"]

getPath g c vis | elem c vis = vis
getPath g c vis | elem c targets = c : vis
getPath g c vis = foldl' (\v ca -> getPath g ca v) (c : vis) (g ! c)

data Switch = Flip !Sta | Conj !(Set String) deriving (Eq)

instance Show Switch where
  show (Flip On) = "on"
  show (Flip Off) = "off"
  show (Conj x) = if Set.null x then "HIGHS" else "somelow"

solveA (Stuff starts m) = do
  let paths = map (\(Signal f _ x) -> reverse $ getPath (Map.map snd m) x mempty) starts
  mprint paths

  -- print $
  --   snd $
  --     flip
  --       runState
  --       m
  --       (replicateM 10000000 (sendPuls starts >> get))

  -- for_ paths $ \p -> do
  --   let p1 = p
  --   -- printlab "p" p
  --   let newM = Map.filterWithKey (\x _ -> elem x p1) m
  --   -- let clean (sigs, Map.map fst -> mm) = map (map (\(Signal from b to) -> ("from", from, mm ! from, "to", to, mm ! to))) sigs

  --   -- print $ foldl1 lcm $ map succ [1881, 2003, 1719, 1775]
  --   mprint $
  --     -- sum $
  --     -- map head $
  --     map snd $
  --       -- filter (\(x, y) -> elem "sp" $ snd $ m ! x) $
  --       init $
  --         sortOn snd
  --         -- \$ sortOn
  --         --   (flip elemIndex p1 . fst)
  --         $
  --           Map.toList $
  --             Map.map (fmap length . drop 2 . take 4 . group) $
  --               Map.unionsWith (++) $
  --                 map (Map.map $ (: []) . fst) $
  --                   fst $
  --                     flip
  --                       runState
  --                       newM
  --                       (replicateM 10000 (sendPuls starts >> get))

  -- print p1

  printlab "length" $
    -- length $
    length $
      filter id $
        take 100000 $
          fst $
            flip
              runState
              m
              (replicateM 10000000 (sendPuls starts))

-- mprint $ map (Map.map fst . Map.filterWithKey (\k _ -> elem k p1)) $ fst $ flip runState m $ replicateM 100 (sendPuls starts >> get)
-- let fm =

-- let parts = flip map starts (\(Signal _ _ to) -> Set.insert to $ getPath (Map.map snd m) to mempty)
-- mprint $ parts

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
  resA

-- resA @=? 1715
-- let resB = solveB parsed
-- print resB
-- resB @=? 1739
