module Day02.Mod where

import Data.List.Split
import qualified Data.Vector.Unboxed as V
import PseudoMacros (__FILE__)
import Utils.Mod (getExampleInputFile, getInputFile, readInputLines, removeEmptyString)

type ProgramInput = (V.Vector Int)

data Program = Program
  { input :: ProgramInput,
    pc :: Int -- The current index to run in the program
  }

type Argument = Int

type ResultLoc = Int

data Op = Add Argument Argument ResultLoc | Mult Argument Argument ResultLoc | Halt deriving (Show)

parseOp :: [Int] -> Op
parseOp (1 : arg1 : arg2 : resLoc : _) = Add arg1 arg2 resLoc
parseOp (2 : arg1 : arg2 : resLoc : _) = Mult arg1 arg2 resLoc
parseOp (99 : rest) = Halt
parseOp _ = error "Error parsing op"

getSlice :: ProgramInput -> Int -> [Int]
getSlice vec amount = V.toList (V.drop amount vec)

updatePos :: Int -> Int -> Program -> Program
updatePos val loc Program {input = input, pc = pc} = Program {input = input V.// [(loc, val)], pc = pc}

updatePosAndPc :: Int -> Int -> Int -> Program -> Program
updatePosAndPc val loc pcIncr Program {input = input, pc = pc} = Program {input = input V.// [(loc, val)], pc = pc + pcIncr}

doOp :: Program -> (Int -> Int -> Int) -> Int -> Int -> Int
doOp Program {input = input} op pos1 pos2 = op val1 val2
  where
    val1 = (V.!) input pos1
    val2 = (V.!) input pos2

runStep :: Program -> Program
runStep prog@Program {input = input, pc = pc} =
  case op of
    Add a1 a2 res -> updatePosAndPc (doOp prog (+) a1 a2) res 4 prog
    Mult a1 a2 res -> updatePosAndPc (doOp prog (*) a1 a2) res 4 prog
    Halt -> Program {input = input, pc = -1}
  where
    op = parseOp (getSlice input pc)

runProgram :: Program -> Program
runProgram prog =
  case stepRes of
    Program {pc = -1} -> stepRes
    _ -> runProgram stepRes
  where
    stepRes = runStep prog

getOutput :: Program -> Int
getOutput Program {input = input} = (V.!) input 0

getProg :: IO Program
getProg = do
  input <- readInputLines $ getInputFile $__FILE__
  let flatInput = concatMap (removeEmptyString . splitOn ",") input
  let programInputLst = map read flatInput :: [Int]
  return Program {input = V.fromList programInputLst, pc = 0}

part1 :: IO ()
part1 = do
  prog <- getProg
  let updated = updatePos 12 1 (updatePos 2 2 prog)
  print $ getOutput $ runProgram updated
  return ()

findNounVerb :: [(Int, Int)] -> Program -> (Int, Int)
findNounVerb [] _ = error "Not found"
findNounVerb ((noun, verb) : xs) prog =
  case output of
    19690720 -> (noun, verb)
    _ -> findNounVerb xs prog
  where
    updated = updatePos noun 1 (updatePos verb 2 prog)
    output = getOutput $ runProgram updated

part2 :: IO ()
part2 = do
  prog <- getProg
  let possibleNounVerbs = [(x, y) | x <- [0 .. 99], y <- [0 .. 99]]
  print $ findNounVerb possibleNounVerbs prog
  return ()

dispatch :: [(Int, IO ())]
dispatch = [(1, part1), (2, part2)]