module Util.Interval.Split (
    Split
    -- * Construction
  , empty
  , modify
  , mergeAdjacentIf
    -- * Query
  , size
  , toList
  , keysSet
  , lookup
  ) where

import Prelude hiding (lookup)

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)

import Util.Interval (Interval)
import Util.Interval qualified as I

-- | Map intervals to values
--
-- Invariant: the intervals do not overlap.
--
-- In order to preserve the invariant, we may have to split intervals when
-- inserting new values into the map; hence the name.
newtype Split v a = Split { toMap :: Map (Interval v) a }
  deriving (Show, Functor)

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

empty :: Split v a
empty = Split Map.empty

modify ::
    (Ord v, Enum v)
  => a         -- ^ Initial value
  -> (a -> a)  -- ^ Update (applied to initial value if no value present)
  -> Interval v -> Split v a -> Split v a
modify e f i (Split im) = Split $
    let (intersect_i, rest) = Map.partitionWithKey (const . I.intersects i) im
    in Map.union (split e f i (Map.toList intersect_i)) rest

mergeAdjacentIf :: forall v a.
     Ord v
  => ((Interval v, a) -> (Interval v, a) -> Maybe (Interval v, a))
  -> Split v a -> Split v a
mergeAdjacentIf f =
    Split . Map.fromList . go . toList
  where
    go :: [(Interval v, a)] -> [(Interval v, a)]
    go []  = []
    go [x] = [x]
    go (x : y : zs)
      | Just xy' <- f x y = go (xy' : zs)
      | otherwise         = x : go (y : zs)

{-------------------------------------------------------------------------------
  Query
-------------------------------------------------------------------------------}

size :: Split v a -> Int
size = Map.size . toMap

toList :: Split v a -> [(Interval v, a)]
toList = Map.toList . toMap

keysSet :: Split v a -> Set (Interval v)
keysSet = Map.keysSet . toMap

lookup :: Ord v => Interval v -> Split v a -> Maybe a
lookup i = Map.lookup i . toMap

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

split :: forall v a.
     (Ord v, Enum v)
  => a                   -- ^ Initial value
  -> (a -> a)            -- ^ Update function
  -> Interval v          -- ^ Interval to insert
  -> [(Interval v, a)]   -- ^ Intersecting intervals
  -> Map (Interval v) a
split e f = \i -> Map.fromList . go i
  where
    -- PRE1:  all intervals in the list intersect with @i@
    -- PRE2:  the intervals in the list are non-empty and do not overlap
    -- PRE3:  i is non-empty
    -- POST1: function @f@ has been applied on (and only on) interval @i@
    -- POST2: the intervals in the result are non-empty and do not overlap
    go :: Interval v -> [(Interval v, a)] -> [(Interval v, a)]
    go i [] = [(i, f e)]
    go i js@((j, a) : js')
      --
      -- Rule out impossible cases
      --

      -- >  {...i...}
      -- >             {...j...}
      | I.high i < I.low j =
          error "split: PRE1.a"
      -- >             {...i...}
      -- >  {...j...}
      | I.high j < I.low i =
          error "split: PRE1.b"

      --
      -- Cases where one interval starts before the other
      --

      -- >  {...i...}     or  {...i...}
      -- >     {...j...}        {.j.}
      | I.low i < I.low j =
          -- > low i <= pred (low j)   (from guard)
          -- > low j <= high i         (from PRE1.a)
            (I.nonEmpty (I.low i) (pred (I.low j)), f e)
          : go (I.nonEmpty (I.low j) (I.high i)) js
      -- >     {...i...}  or    {.i.}
      -- >  {...j...}         {...j...}
      | I.low j < I.low i =
          -- > low j <= pred (low i)   (from guard)
          -- > low i <= high j         (from PRE1.b)
            (I.nonEmpty (I.low j) (pred (I.low i)), a)
          : go i ((I.nonEmpty (I.low i) (I.high j), a) : js')

      --
      -- At this point we have established the lower bounds are equal
      --

      -- > {.i.}
      -- > {...j...}
      | I.high i < I.high j =
          -- > low j         <= high i   (from PRE1.a)
          -- > succ (high i) <= high j   (from guard)
            (I.nonEmpty (I.low j) (I.high i), f a)
          : (I.nonEmpty (succ (I.high i)) (I.high j), a)
          : js'

      -- >  {...i...}
      -- >  {.j.}
      | I.high j < I.high i =
           -- > low i         <= high j   (from PRE1.b)
           -- > succ (high j) <= high i   (from guard)
            (I.nonEmpty (I.low i) (I.high j), f a)
          : go (I.nonEmpty (succ (I.high j)) (I.high i)) js'

      -- high j == high i
      -- > {..i..}
      -- > {..j..}
      | otherwise =
          (i, f a) : js'
