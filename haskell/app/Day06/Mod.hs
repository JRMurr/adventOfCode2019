{-# LANGUAGE OverloadedStrings #-}

module Day06.Mod where

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import PseudoMacros (__FILE__)
import Text.Megaparsec (many, satisfy)
import Utils.Mod

parseName :: Parser String
parseName = many (satisfy (')' /=))

parseOrbit :: Parser (String, String)
parseOrbit = (,) <$> parseName <* ")" <*> parseName

part1 :: IO ()
part1 = do
  input <- readParsedLines (getInputFile $__FILE__) parseOrbit
  let paths = Map.fromList [(y, x : Map.findWithDefault [] x paths) | (x, y) <- input]
  print $ sum (length <$> paths)
  return ()

getTransferLength :: [String] -> [String] -> Int
-- lists start at root so strip the lists until they diverge
getTransferLength (x : xs) (y : ys) | x == y = getTransferLength xs ys
getTransferLength xs ys = length xs + length ys

part2 :: IO ()
part2 = do
  input <- readParsedLines (getInputFile $__FILE__) parseOrbit
  let paths = Map.fromList [(y, x : Map.findWithDefault [] x paths) | (x, y) <- input]
  let myPath = paths Map.! "YOU"
  let santaPath = paths Map.! "SAN"
  print $ getTransferLength (reverse myPath) (reverse santaPath)
  return ()

dispatch :: [(Int, IO ())]
dispatch = [(1, part1), (2, part2)]