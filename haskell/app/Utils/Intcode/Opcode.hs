{-# LANGUAGE DeriveTraversable #-}

module Utils.Intcode.Opcode where

-- | Parameter modes
data Mode
  = -- | absolute position
    Abs
  | -- | immediate
    Imm
  deriving (Eq, Ord, Read, Show)

-- | Opcodes parameterized over argument representations. This allows the transformations form Opcode of mode to Opcode of ints
data Opcode a
  = -- | __addition:__        @c = a + b@
    Add !a !a !a
  | -- | __multiplication:__  @c = a * b@
    Mul !a !a !a
  | -- | __input:__           @a = input()@
    Inp !a
  | -- | __output:__          @output(a)@
    Out !a
  | -- | __halt__
    Hlt
  deriving (Eq, Ord, Read, Show, Functor, Foldable)

-- | Decode an instruction to determine the opcode and parameter modes.
--
-- >>> decode 1002
-- Just (Mul Abs Imm Abs)
decode ::
  -- | opcode
  Int ->
  Maybe (Opcode Mode)
decode n =
  case n `rem` 100 of
    1 -> fill (Add 1 2 3)
    2 -> fill (Mul 1 2 3)
    3 -> fill (Inp 1)
    4 -> fill (Out 1)
    99 -> fill Hlt
    _ -> Nothing
  where
    fill = traverse (parameter n)
{-# INLINEABLE decode #-}

-- | Compute the parameter mode for an argument at a given position.
parameter ::
  -- | opcode
  Int ->
  -- | position
  Int ->
  Maybe Mode
parameter n i =
  case digit (i + 1) n of
    0 -> Just Abs
    1 -> Just Imm
    _ -> Nothing

-- | Arguments visited from left to right.
instance Traversable Opcode where
  {-# INLINE traverse #-}
  traverse f o =
    case o of
      Add x y z -> Add <$> f x <*> f y <*> f z
      Mul x y z -> Mul <$> f x <*> f y <*> f z
      Inp x -> Inp <$> f x
      Out x -> Out <$> f x
      Hlt -> pure Hlt

-- | Extract the ith digit from a number.
--
-- >>> digit 0 2468
-- 8
-- >>> digit 3 2468
-- 2
-- >>> digit 4 2468
-- 0
digit ::
  -- | position
  Int ->
  -- | number
  Int ->
  -- | digit
  Int
digit i x = x `quot` (10 ^ i) `rem` 10