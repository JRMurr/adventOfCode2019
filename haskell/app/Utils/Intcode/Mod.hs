{-# LANGUAGE OverloadedStrings #-}

module Utils.Intcode.Mod
  ( -- * Simple list interface
    getIntcodeInput,

    -- * Machine state
    Machine,
    (!),
    new,
    set,
    -- memoryList,

    -- * Small-step semantics
    Step (..),
    step,

    -- * Exceptions
    IntcodeFault (..),
  )
where

import Control.Applicative (many)
import Control.Exception (Exception (..), throw, throwIO)
import Data.Traversable (mapAccumL)
import Debug.Trace (trace)
import Text.Megaparsec (eof, sepBy)
import Text.Megaparsec.Char (newline)
import Text.Show.Functions ()
import Utils.Intcode.Machine
import Utils.Intcode.Opcode
import Utils.Mod (Parser, getParsedInput, number)

memoryParser :: Parser [Int]
memoryParser = number `sepBy` ","

getIntcodeInput :: FilePath -> IO [Int]
getIntcodeInput fp = getParsedInput fp (memoryParser <* many newline <* eof)

-- | Result of small-step semantics.
data Step
  = -- | no effect
    Step !Machine
  | -- | output
    StepOut !Int !Machine
  | -- | input
    StepIn (Int -> Machine)
  | -- | halt
    StepHalt
  | -- | bad instruction
    StepFault
  deriving (Show)

-- | Small-step semantics of virtual machine.
step :: Machine -> Step
step mach =
  case populateParams <$> decode (mach ! pc mach) of
    Nothing -> StepFault
    Just (pc', opcode) -> opcodeImpl opcode $! jmp pc' mach
  where
    populateParams :: Opcode Mode -> (Int, Opcode Int)
    populateParams = mapWithIndex toPtr (pc mach + 1)

    toPtr :: Int -> Mode -> Int
    toPtr i Imm = i -- This i is the address to the value we need
    toPtr i Abs = mach ! i -- the address we need is stored at index i

-- | Apply a decoded opcode to the machine state.
opcodeImpl ::
  -- | opcode with pointers
  Opcode Int ->
  -- | machine with PC updated
  Machine ->
  Step
opcodeImpl o m =
  trace ("OPCODE: " ++ show o) $
    case o of
      Add a b c -> Step (set c (at a + at b) m)
      Mul a b c -> Step (set c (at a * at b) m)
      Inp a -> StepIn (\i -> set a i m)
      Out a -> StepOut (at a) m
      Hlt -> StepHalt
  where
    at i = m ! i

mapWithIndex :: (Int -> a -> b) -> Int -> Opcode a -> (Int, Opcode b)
mapWithIndex f = mapAccumL (\i a -> (i + 1, f i a))
{-# INLINE mapWithIndex #-}

-- | Error when a machine fails to decode an instruction.
data IntcodeFault = IntcodeFault
  deriving (Eq, Ord, Show, Read)

instance Exception IntcodeFault where
  displayException _ = "intcode machine fault"