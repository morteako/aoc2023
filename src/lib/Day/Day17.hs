module Day.Day17 (run) where

import Control.Arrow ((>>>))
import Control.Lens
import Control.Monad (void)
import Control.Monad.State
import Data.Char
import Data.Foldable
import Data.List.Extra
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes, mapMaybe)
import Data.Semigroup
import Data.Set hiding (fold)
import Data.Set qualified as Set hiding (fold)
import Data.Tuple.Extra (fst3, snd3)
import Debug.Trace
import GHC.Records
import Linear (V2 (..))
import Test.HUnit ((@=?))
import Text.RawString.QQ (r)
import Utils

parse :: String -> Map (V2 Int) Int
parse = parseAsciiMap (Just . digitToInt)

data Dir = N | S | W | E deriving (Show, Eq, Ord, Enum, Bounded)

dirToVec :: Dir -> V2 Int
dirToVec x = case x of
  N -> V2 0 (-1)
  S -> V2 0 1
  E -> V2 1 0
  W -> V2 (-1) 0

isOpp N S = True
isOpp S N = True
isOpp W E = True
isOpp E W = True
isOpp _ _ = False

d .+ v = dirToVec d + v

-- instance HasField "move"

-- doMoves :: Map (V2 Int) Int -> V2 Int -> Dir -> State (Set (V2 Int)) ()
-- doMoves grid goal curPos dir dir
type S = Map (V2 Int, Dir, Int) Int

nextDir dirCount dir newDir
  | isOpp dir newDir = Nothing
  | dir == newDir && dirCount == 3 = Nothing
  | dir == newDir = Just (newDir, dirCount + 1)
  | otherwise = Just (newDir, 1)

doMoves :: V2 Int -> Map (V2 Int) Int -> _ -> V2 Int -> Dir -> Int -> State S (Maybe Int)
doMoves _ _ vis curPos dir dirC | Set.member curPos vis = dtraceShow ("simple", curPos, dir, dirC) pure Nothing
doMoves goal grid _ curPos _ _ | curPos == goal = pure $ Map.lookup goal grid
doMoves goal grid vis curPos dir dirC = case Map.lookup curPos grid of
  Nothing -> pure Nothing -- outside grid
  Just val -> do
    seen <- get
    case Map.lookup (curPos, dir, dirC) seen of
      Just minC -> pure $ Just minC
      Nothing -> do
        let newvis = Set.insert curPos vis
        -- let ds = move t dir
        let newDirs = [N .. E]
        let newDirCounts = mapMaybe (nextDir dirC dir) newDirs
        let f (d, c) = doMoves goal grid newvis (d .+ curPos) d c
        minOfRest <- catMaybes <$> traverse f newDirCounts
        case minOfRest of
          [] -> do
            pure Nothing
          _ -> do
            let mii = val + minimum minOfRest
            modify $ dtraceShow (curPos, dir, dirC, "mii", mii) (Map.insert (curPos, dir, dirC) mii)
            pure $ Just mii

-- DEBUG ONLY
doMovesSpecificSet :: V2 Int -> Map (V2 Int) Int -> _ -> V2 Int -> Dir -> Int -> State S (Maybe Int)
doMovesSpecificSet _ _ vis curPos dir dirC | Set.member (curPos, dir, dirC) vis = dtraceShow ("spec", curPos, dir, dirC) pure Nothing
doMovesSpecificSet goal grid _ curPos _ _ | curPos == goal = pure $ Map.lookup goal grid
doMovesSpecificSet goal grid vis curPos dir dirC = case Map.lookup curPos grid of
  Nothing -> pure Nothing -- outside grid
  Just val -> do
    seen <- get
    case Map.lookup (curPos, dir, dirC) seen of
      Just minC -> pure $ Just minC
      Nothing -> do
        let newvis = Set.insert (curPos, dir, dirC) vis
        -- let ds = move t dir
        let newDirs = [N .. E]
        let newDirCounts = mapMaybe (nextDir dirC dir) newDirs
        let f (d, c) = doMovesSpecificSet goal grid newvis (d .+ curPos) d c
        minOfRest <- catMaybes <$> traverse f newDirCounts
        case minOfRest of
          [] -> do
            pure Nothing
          _ -> do
            let mii = val + minimum minOfRest
            modify $ dtraceShow (curPos, dir, dirC, "mii", mii) (Map.insert (curPos, dir, dirC) mii)
            pure $ Just mii

dtraceShow s a = if debug then traceShow s a else a
dtraceLab s a = if debug then traceLab s a else a

-- testInput =
--   [r|
-- 48887735
-- 74655533

-- | ]

-- CORRECT ish?
-- " 51" " 47" " 39" " 31" " 23" " 16" "  9" "  8"
-- " 53" " 43" " 37" " 29" " 16" " 11" "  6" "   "

-------- WRONG
-- " 51" " 47" " 39" " 31" " 23" " 16" "  9" "  8"
-- " 53" " 43" " 37" " 29" " 16" " 11" "  6" "   "
debug = False

solveA :: Map (V2 Int) Int -> IO ()
solveA grid = do
  printlab "start" startPos
  printlab "end" end
  for_ [doMoves end grid mempty, doMovesSpecificSet end grid mempty] $ \f -> do
    let res = f startPos E (-1)
    let (Just minRes, minMap) = flip runState mempty res
    print $ minRes - startVal
    -- print $ Set.map show $ Map.keysSet minMap

    let mini = Map.mapKeysWith min fst3 minMap
    let fixed = fmap (\x -> takeEnd 3 (replicate 6 ' ' ++ x)) $ Map.union (fmap show mini) (" " <$ grid)
    -- printV2Map $ fmap show mini
    printV2Map $ fixed
 where
  -- print 1

  -- printV2Map $ snd stat

  (startPos, startVal) = Map.findMin grid
  end = fst $ Map.findMax grid

solveB = id

testInputOrg =
  [r|2413432311323
3215453535623
3255245654254
3446585845452
4546657867536
1438598798454
4457876987766
3637877979653
4654967986887
4564679986453
1224686865563
2546548887735
4322674655533
|]

testInput =
  [r|2546548887735
4322674655533
|]

testInput2 =
  [r|12
12
|]

fasit =
  [r|2>>34^>>>1323
32v>>>35v5623
32552456v>>54
3446585845v52
4546657867v>6
14385987984v4
44578769877v6
36378779796v>
465496798688v
456467998645v
12246868655<v
25465488877v5
43226746555v>
|]

printFasit = do
  let p = parseAsciiMap (\x -> if elem x (">v^<" :: String) then Just x else Nothing) fasit
  -- printV2Map p
  mprint $ Map.keys p

-- 2>>34^>>>1323
-- 32v>>>35v5623
-- 32552456v>>54
-- 3446585845v52
-- 4546657867v>6
-- 14385987984v4
-- 44578769877v6
-- 36378779796v>
-- 465496798688v
-- 456467998645v
-- 12246868655<v
-- 25465488877v5
-- 43226746555v>

-- "    105" "    103" "     99" "     97" "     94" "     84" "     79" "     77" "     74" "     73" "     73" "     70" "     70"
-- "    105" "    102" "     99" "     96" "     91" "     87" "     82" "     82" "     75" "     72" "     70" "     68" "     67"
-- "    107" "    104" "    103" "     98" "     93" "     94" "     87" "     80" "     72" "     67" "     64" "     65" "     64"
-- "    107" "    108" "    107" "    104" "     98" "     95" "     86" "     75" "     67" "     63" "     58" "     58" "     56"
-- "    104" "    104" "    104" "    106" "    100" "     94" "     88" "     78" "     70" "     64" "     54" "     53" "     54"
-- "    100" "     99" "     95" "    103" "     94" "     90" "     82" "     75" "     68" "     59" "     49" "     50" "     47"
-- "     99" "     96" "     92" "     92" "     89" "     81" "     74" "     68" "     59" "     51" "     44" "     38" "     39"
-- "     91" "     93" "     87" "     85" "     83" "     75" "     68" "     61" "     52" "     44" "     37" "     32" "     31"
-- "     88" "     90" "     84" "     78" "     79" "     69" "     61" "     54" "     45" "     35" "     31" "     27" "     27"
-- "     84" "     84" "     81" "     74" "     70" "     64" "     57" "     47" "     38" "     29" "     23" "     19" "     14"
-- "     80" "     79" "     77" "     71" "     67" "     59" "     48" "     40" "     31" "     25" "     20" "     15" "     11"
-- "     82" "     83" "     78" "     71" "     65" "     57" "     47" "     39" "     31" "     23" "     16" "      9" "      8"
-- "     82" "     78" "     75" "     66" "     62" "     53" "     43" "     37" "     31" "     16" "     11" "      6" "       "

-- --------
-- "    104" "    102" "     97" "     93" "     90" "     82" "     79" "     77" "     74" "     73" "     72" "     69" "     70"
-- "    103" "    100" "     97" "     96" "     91" "     86" "     81" "     78" "     73" "     70" "     67" "     66" "     65"
-- "    105" "    102" "    102" "     97" "     92" "     87" "     83" "     76" "     70" "     65" "     61" "     63" "     62"
-- "    108" "    106" "    106" "    103" "     97" "     92" "     83" "     75" "     67" "     63" "     58" "     58" "     56"
-- "    103" "    105" "    104" "    106" "    100" "     93" "     88" "     75" "     67" "     61" "     54" "     50" "     51"
-- "     97" "    100" "     94" "    102" "     95" "     91" "     80" "     77" "     67" "     65" "     49" "     47" "     45"
-- "     94" "     96" "     91" "     92" "     90" "     82" "     72" "     70" "     58" "     51" "     44" "     38" "     39"
-- "     86" "     91" "     86" "     85" "     83" "     75" "     66" "     61" "     50" "     44" "     37" "     32" "     31"
-- "     83" "     85" "     83" "     78" "     79" "     68" "     59" "     52" "     43" "     35" "     31" "     27" "     27"
-- "     79" "     79" "     78" "     74" "     70" "     64" "     57" "     47" "     38" "     29" "     23" "     19" "     14"
-- "     75" "     74" "     72" "     70" "     66" "     59" "     48" "     40" "     31" "     25" "     20" "     15" "     11"
-- "     79" "     78" "     74" "     66" "     60" "     55" "     47" "     39" "     31" "     23" "     16" "      9" "      8"
-- "     77" "     73" "     70" "     66" "     62" "     56" "     43" "     37" "     29" "     16" "     11" "      6" "       "

run :: String -> IO ()
run input = void $ do
  -- input <- putStrLn "#####    testInput   #####" >> pure testInput
  print input
  let parsed = parse input
  -- print parsed
  let resA = solveA parsed
  resA

-- printFasit

-- print "hopp"

-- print $ fold [Just (Min 1), Nothing]

-- resA @=? 1715
-- let resB = solveB parsed
-- print resB
-- resB @=? 1739
