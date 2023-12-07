{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Day.Day05 (run) where

import Control.Arrow ((>>>))

import Data.IntervalMap (Interval (IntervalCO))
import Data.IntervalMap.Generic.Interval (Interval (..))
import Data.List
import Data.List.Split (splitOn)
import GHC.Records (HasField (getField))
import Test.HUnit ((@=?))
import Prelude

data Mapping = Mapping {destStart, sourceStart, range :: Integer}

parse :: [Char] -> ([Integer], [[Mapping]])
parse = splitOn "\n\n" >>> map lines >>> parseSeedsAndMaps
 where
  parseSeedsAndMaps ([seeds] : maps) = (map (read @Integer) (tail $ words seeds), map parseMappings maps)
  parseMappings (_name : lines) = map toMapping $ map parseMapping $ lines
  parseMapping = words >>> map (read @Integer)
  toMapping [a, b, c] = Mapping a b c
  toMapping xs = error $ show xs

data Func = Func {adder :: Integer, min :: Integer, max :: Integer}

mapToFunc :: Mapping -> Func
mapToFunc m = Func (m.destStart - m.sourceStart) m.sourceStart (m.sourceStart + m.range)

unpackFuncs :: [Func] -> Integer -> Integer
unpackFuncs = go . sortOn (.min)
 where
  go :: [Func] -> (Integer -> Integer)
  go [] x = x
  go (Func adder min max : fs) x | x >= min && x < max = x + adder
  go (_ : fs) x = unpackFuncs fs x

solveA :: ([Integer], [[Mapping]]) -> Integer
solveA (seeds, mappings) = minimum $ map combinedFunc seeds
 where
  combinedFunc = foldr1 (.) $ reverse $ map (unpackFuncs . map mapToFunc) mappings
  funcs = map (unpackFuncs . map mapToFunc) mappings

data Splitting = Before | OverlapLeft | RangeSubsumes | FuncSubsumes | OverlapRight | After | Equal | None deriving (Show)

makeSplitting :: Range -> Range -> Splitting
makeSplitting range func
  | range == func = Equal
  | before func range = Before
  | after func range = After
  | subsumes range func = RangeSubsumes
  | subsumes func range = FuncSubsumes
  | overlaps range func && func < range = OverlapLeft
  | overlaps range func && func > range = OverlapRight
  | otherwise = None

type Range = Data.IntervalMap.Interval Integer

instance HasField "lower" Range Integer where
  getField :: Range -> Integer
  getField = lowerBound

instance HasField "upper" Range Integer where
  getField :: Range -> Integer
  getField = upperBound

applyRanges :: [Range] -> [(Range, Integer)] -> [Range]
applyRanges range [] = range
applyRanges [] _fs = []
applyRanges (range : restRanges) fs@((funcRange, adder) : restFuncs) = case makeSplitting range funcRange of
  _ | isEmpty range -> applyRanges restRanges fs
  _ | isEmpty funcRange -> applyRanges (range : restRanges) restFuncs
  Before -> applyRanges (range : restRanges) restFuncs
  After -> range : applyRanges restRanges fs
  Equal -> fmap (+ adder) range : applyRanges restRanges restFuncs
  OverlapLeft ->
    let
      newRangeLeft = fmap (+ adder) $ IntervalCO range.lower funcRange.upper
      modifiedRange = IntervalCO funcRange.upper range.upper
     in
      newRangeLeft : applyRanges (modifiedRange : restRanges) restFuncs
  OverlapRight ->
    let
      plainRange = IntervalCO range.lower funcRange.lower
      addedRange = fmap (+ adder) $ IntervalCO funcRange.lower range.upper
     in
      plainRange : addedRange : applyRanges restRanges fs
  RangeSubsumes ->
    let
      newRangeLeft = IntervalCO range.lower funcRange.lower
      addedRange = fmap (+ adder) funcRange
      modifiedRange = IntervalCO funcRange.upper range.upper
     in
      newRangeLeft : addedRange : applyRanges (modifiedRange : restRanges) restFuncs
  FuncSubsumes ->
    let
      addedRange = fmap (+ adder) range
     in
      addedRange : applyRanges restRanges fs

getSeedRanges :: [Integer] -> [Range]
getSeedRanges (val : range : rest) = IntervalCO val (val + range) : getSeedRanges rest
getSeedRanges [] = []

solveB :: ([Integer], [[Mapping]]) -> Integer
solveB (seeds, mappings) = minimum $ map (.lower) $ foldl' app seedRanges mappingRanges
 where
  app r rs = sort $ applyRanges r rs

  seedRanges = sort (getSeedRanges seeds)

  mappingRanges = map (sort . map (funcToAdder . mapToFunc)) mappings

funcToAdder m = (IntervalCO m.min m.max, m.adder)

run :: String -> IO ()
run input = do
  let parsed = parse input
  let resA = solveA parsed
  print resA
  resA @=? 579439039
  let resB = solveB parsed
  print resB
  resB @=? 7873084
