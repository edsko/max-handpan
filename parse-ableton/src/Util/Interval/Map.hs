module Util.Interval.Map (
    toList
  , fromList
  ) where

import Data.IntervalMap.FingerTree (IntervalMap, Interval(..))
import Data.IntervalMap.FingerTree qualified as IM

import Util

-- | Convert 'IntervalMap' to list, preserving intervals
toList :: Ord v => IntervalMap v a -> [(Interval v, a)]
toList xs =
    case IM.leastView xs of
      Nothing       -> []
      Just (x, xs') -> x : toList xs'

fromList :: Ord v => [(Interval v, a)] -> IntervalMap v a
fromList as = repeatedly (uncurry IM.insert) as IM.empty
