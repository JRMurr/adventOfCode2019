-- needed for the "-" in the parser
{-# LANGUAGE OverloadedStrings #-}

module Day04.Mod where

import Data.List
import PseudoMacros (__FILE__)
import Utils.Mod

-- | Parses two numbers separated by a dash.
rangeParser :: Parser (Int, Int)
rangeParser = (,) <$> number <* "-" <*> number

nondecreasing :: Ord a => [a] -> Bool
nondecreasing xs = and (zipWith (<=) xs (tail xs))

-- Get the length of each adjacent digits that are the same
runs :: Eq a => [a] -> [Int]
runs = map length . group

-- convert to strings to look at digits more easily
getNumsInRange :: (Int, Int) -> [String]
getNumsInRange (lo, hi) = map show [lo .. hi]

part1 :: IO ()
part1 = do
  input <- readFile $ getInputFile $__FILE__
  let [range] = getParsedLines input rangeParser
  let nums = map runs $ filter nondecreasing $ getNumsInRange range
  print (count (any (> 1)) nums)
  return ()

part2 :: IO ()
part2 = do
  input <- readFile $ getInputFile $__FILE__
  let [range] = getParsedLines input rangeParser
  let nums = map runs $ filter nondecreasing $ getNumsInRange range
  print (count (elem 2) nums)
  return ()

dispatch :: [(Int, IO ())]
dispatch = [(1, part1), (2, part2)]