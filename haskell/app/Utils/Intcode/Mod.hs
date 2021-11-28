{-# LANGUAGE OverloadedStrings #-}

module Utils.Intcode.Mod
  ( -- * Simple list interface
    intcodeToList,
    getIntcodeInput,

    -- * Machine state
    Machine,
    (!),
    new,
    set,

    -- * Big-step semantics
    Effect (..),
    run,

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

------------------------------------------------------------------------
-- High-level interface
------------------------------------------------------------------------

-- | Init an intcode machine with the given memory and inputs. Returns list of outputs
intcodeToList ::
  -- | initial memory
  [Int] ->
  -- | inputs
  [Int] ->
  -- | outputs
  [Int]
intcodeToList = effectList . run . new

-- | Evaluate a program's effect as a function from a list of
-- inputs to a list of outputs.
--
-- Throws: 'IntcodeFault' when machine faults or too few inputs are provided.
effectList ::
  -- | program effect
  Effect ->
  -- | inputs
  [Int] ->
  -- | outputs
  [Int]
effectList effect inputs =
  case effect of
    Fault -> throw IntcodeFault
    Halt -> []
    Output o e -> o : effectList e inputs
    Input f ->
      case inputs of
        x : xs -> effectList (f x) xs
        [] -> throw IntcodeFault

-- | Possible effects from running a machine
data Effect
  = -- | Output an integer
    Output !Int Effect
  | -- | Input an integer
    Input (Int -> Effect)
  | -- | Halt execution
    Halt
  | -- | Execution failure
    Fault
  deriving (Show)

-- | Big-step semantics of virtual machine. The implementation details
-- of 'Machine' are abstracted away and the program behavior can be
-- observed by interpreting the various 'Effect' constructors.
--
-- >>> run (new [1102,34915192,34915192,7,4,7,99,0])
-- Output 1219070632396864 Halt
--
-- >>> run (new [3,1,99])
-- Input <function>
run :: Machine -> Effect
run mach =
  case step mach of
    Step mach' -> run mach'
    StepOut out mach' -> Output out (run mach')
    StepIn f -> Input (run . f)
    StepHalt -> Halt
    StepFault -> Fault

-- | Compose two effects together. Outputs from first argument are
-- used as inputs to the second effect. Composed effect halts when
-- the second machine halts.
--
-- >>> let mult n = Input (\i -> Output (i*n) Halt)
-- >>> let add  n = Input (\i -> Output (i+n) Halt)
-- >>> effectList (mult 3 >>> add 1) [4]
-- [13]
(>>>) :: Effect -> Effect -> Effect
_ >>> Fault = Fault
_ >>> Halt = Halt
x >>> Output o e = Output o (x >>> e)
x >>> Input g = input x
  where
    input Fault = Fault
    input Halt = Fault
    input (Output o e) = e >>> g o
    input (Input f) = Input (input . f)

infixl 9 >>>

-- | Run first effect until it halts, then run the second effect.
--
-- >>> Output 1 Halt `followedBy` Output 2 Halt
-- Output 1 (Output 2 Halt)
--
-- >>> Output 1 Halt `followedBy` Fault
-- Output 1 Fault
--
-- >>> Fault `followedBy` undefined
-- Fault
followedBy :: Effect -> Effect -> Effect
followedBy Halt y = y
followedBy Fault _ = Fault
followedBy (Output o x) y = Output o (followedBy x y)
followedBy (Input f) y = Input (\i -> followedBy (f i) y)

-- | Provide an input to the first occurrence of an input request
-- in a program effect. It is considered a fault if a program
-- terminates before using the input.
--
-- >>> feedInput [5,6] (Input (\x -> Input (\y -> Output (x*y) Halt)))
-- Output 30 Halt
--
-- >>> feedInput [7] Halt
-- Fault
feedInput ::
  -- | inputs
  [Int] ->
  Effect ->
  Effect
feedInput [] e = e
feedInput xs (Output o e) = Output o (feedInput xs e)
feedInput (x : xs) (Input f) = feedInput xs (f x)
feedInput _ _ = Fault

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
  -- trace ("OPCODE: " ++ show o) $
  case o of
    Add a b c -> Step (set c (at a + at b) m)
    Mul a b c -> Step (set c (at a * at b) m)
    Inp a -> StepIn (\i -> set a i m)
    Out a -> StepOut (at a) m
    Jnz a b -> Step (if at a /= 0 then jmp (at b) m else m)
    Jz a b -> Step (if at a == 0 then jmp (at b) m else m)
    Lt a b c -> Step (set c (if at a < at b then 1 else 0) m)
    Eq a b c -> Step (set c (if at a == at b then 1 else 0) m)
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