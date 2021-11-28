module Day05.Mod where

import PseudoMacros (__FILE__)
import Utils.Intcode.Mod
import Utils.Mod

getMachine :: IO Machine
getMachine = new <$> getIntcodeInput (getExampleInputFile $__FILE__)

part1 :: IO ()
part1 = do
  pgm <- getMachine
  return ()

part2 :: IO ()
part2 = do
  input <- readInputLines $ getInputFile $__FILE__
  putStrLn "part2"
  return ()

dispatch :: [(Int, IO ())]
dispatch = [(1, part1), (2, part2)]