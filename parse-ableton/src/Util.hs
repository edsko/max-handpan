module Util (
    repeatedly
  , splitOn
  , splitSortedOn
  , splitFst
  , mergeAdjacent
  , allEqualUpTo
    -- * Pretty-printing
  , bracket
  , showSet
  , showMap
  ) where

import Control.Monad (guard)
import Data.List (foldl', sortOn, intercalate)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

repeatedly :: (a -> b -> b) -> ([a] -> b -> b)
repeatedly = flip . foldl' . flip

-- | Like 'groupOn', but providing evidence of the grouping
--
-- Example:
--
-- >    groupSplit id [ (True, 'a'), (True, 'b'), (False, 'c'), (True, 'd') ]
-- > == [ (True, "ab"), (False, "c"), (True, "d") ]
splitOn :: forall x a b. Eq a => (x -> (a, b)) -> [x] -> [(a, [b])]
splitOn f = \case
    []   -> []
    x:xs -> let (a, b) = f x in go a [b] xs
  where
    go :: a -> [b] -> [x] -> [(a, [b])]
    go a bs []     = [(a, reverse bs)]
    go a bs (x:xs) = let (a', b) = f x in
                     if a == a' then                   go a  (b:bs) xs
                                else (a, reverse bs) : go a' [b]    xs

-- | Like 'splitOn', but sort the list first
splitSortedOn :: Ord a => (x -> (a, b)) -> [x] -> [(a, [b])]
splitSortedOn f = splitOn f . sortOn (fst . f)

-- | Specialization of 'splitOn'
splitFst :: Eq a => [(a, b)] -> [(a, [b])]
splitFst = splitOn id

-- | Merge values for adjacent identical keys
mergeAdjacent :: (Eq a, Semigroup b) => [(a, b)] -> [(a, b)]
mergeAdjacent []                        = []
mergeAdjacent [(a, b)]                  = [(a, b)]
mergeAdjacent ((a, b) : (a', b') : rest)
  | a == a'   = mergeAdjacent ((a, b <> b') : rest)
  | otherwise = (a, b) : mergeAdjacent ((a', b') : rest)

allEqualUpTo :: Eq b => (a -> b) -> [a] -> Maybe b
allEqualUpTo f as =
    case map f as of
      []   -> error "allEqualUpTo: empty list"
      b:bs -> guard (all (== b) bs) >> return b

{-------------------------------------------------------------------------------
  Pretty-printing
-------------------------------------------------------------------------------}

bracket :: Char -> Char -> String -> String
bracket l r xs = l : xs ++ [r]

showSet :: (a -> String) -> Set a -> String
showSet f = bracket '[' ']' . intercalate "," . map f . Set.toList

showMap :: forall k v. (k -> String) -> (v -> String) -> Map k v -> String
showMap f g = bracket '[' ']' . intercalate "," . map showElem . Map.toList
  where
    showElem :: (k, v) -> String
    showElem (k, v) = bracket '(' ')' $ intercalate "," [f k, g v]
