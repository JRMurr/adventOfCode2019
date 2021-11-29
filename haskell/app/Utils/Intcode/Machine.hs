module Utils.Intcode.Machine where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.Primitive.PrimArray as P

data Machine = Machine
  { -- | program counter
    pc :: !Int,
    -- | relative base pointer
    relBase :: !Int,
    -- | memory updates
    memUpdates :: !(IntMap Int),
    -- | initial memory
    memInitial :: {-# UNPACK #-} !(P.PrimArray Int)
  }
  deriving (Eq, Ord, Show)

-- | Value stored in initial memory image at given index.
indexImage ::
  -- | machine
  Machine ->
  -- | position
  Int ->
  -- | value
  Int
indexImage m i
  | i < P.sizeofPrimArray a, 0 <= i = P.indexPrimArray a i
  | otherwise = 0
  where
    a = memInitial m
{-# INLINE indexImage #-}

-- | Memory lookup.
(!) ::
  -- | machine
  Machine ->
  -- | position
  Int ->
  -- | value
  Int
m ! i = IntMap.findWithDefault (indexImage m i) i (memUpdates m)
{-# INLINE (!) #-}

-- | Construct machine from a list of initial values starting
-- at address 0. Program counter and relative base start at 0.
new ::
  -- | initial memory
  [Int] ->
  Machine
new initialValues =
  Machine
    { pc = 0,
      relBase = 0,
      memUpdates = IntMap.empty,
      memInitial = P.primArrayFromList initialValues
    }

-- | Store value at given memory position.
set ::
  -- | position
  Int ->
  -- | value
  Int ->
  Machine ->
  Machine
set i v m
  | v == o = m {memUpdates = IntMap.delete i (memUpdates m)} -- the value is being set to what it started with so remove to save space
  | otherwise = m {memUpdates = IntMap.insert i v (memUpdates m)}
  where
    o = indexImage m i

-- | Set program counter to a new address.
jmp ::
  -- | program counter
  Int ->
  Machine ->
  Machine
jmp i mach = mach {pc = i}
{-# INLINE jmp #-}

-- | Add offset to relative base pointer.
addRelBase ::
  -- | offset
  Int ->
  Machine ->
  Machine
addRelBase i mach = mach {relBase = relBase mach + i}
{-# INLINE addRelBase #-}