module LUTs (createLUTs) where

import Prelude hiding (id)

import Control.Monad
import Data.Char (toLower)
import Data.List (sortOn, sort, nub)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Tuple (swap)
import System.FilePath

import Ableton.MultiSampleParts
import Ableton.Schema
import Ableton.Types

import CmdLine

import Util
import Util.Interval (Interval)
import Util.Interval qualified as I
import Util.Interval.Split (Split)
import Util.Interval.Split qualified as Split

createLUTs :: OptionsCreateLUTs -> [MSP] -> IO ()
createLUTs OptionsCreateLUTs{..} msps = do
    writeFile (withSuffix "LUTs.log") $ show luts
    writeFile (withSuffix "rangeIDs.log") $ show rangeIds
    writeFile (withSuffix "statistics.log") $ unlines [
        "# Statistics"
      , "Number of different sample ranges: " ++ show (Map.size sampleIds)
      , "## Sample files used"
      ,   unlines
        . map (\f -> f ++ " (" ++ fileToSymbol f ++ ")")
        . nub
        . sort
        . map (getName . file . fst)
        . Map.toList
        $ sampleIds
      , "## Distribution over the various ranges"
      , "Unique combinations: " ++ show (product [
            Split.size splitSelector
          , Split.size splitKey
          , Split.size splitVelocity
          , Split.size splitChain
          ])
      , "### Selector"
      , show $ Set.size <$> splitSelector
      , "### Key"
      , show $ Set.size <$> splitKey
      , "### Velocity"
      , show $ Set.size <$> splitVelocity
      , "### Articulation"
      , show $ Set.size <$> splitChain
      , "## Ambiguous combinations"
      , show $ overlaps luts
      ]

    -- Write input -> range ID tables
    writeFile (withSuffix "range_id_selector.table") (toRangeId rangesSelector)
    writeFile (withSuffix "range_id_key.table")      (toRangeId rangesKey)
    writeFile (withSuffix "range_id_velocity.table") (toRangeId rangesVelocity)
    writeFile (withSuffix "range_id_chain.table")    (toRangeId rangesChain)

    -- Range IDs -> matching sample ID collections
    writeFile (withSuffix "samples_selector.coll") (matchingRangeId rangesSelector splitSelector)
    writeFile (withSuffix "samples_key.coll")      (matchingRangeId rangesKey      splitKey)
    writeFile (withSuffix "samples_velocity.coll") (matchingRangeId rangesVelocity splitVelocity)
    writeFile (withSuffix "samples_chain.coll")    (matchingRangeId rangesChain    splitChain)

    -- Samples themselves
    writeFile (withSuffix "samples.coll") (sampleLUT sampleRate sampleIds)
  where
    withSuffix :: String -> FilePath
    withSuffix suffix = outputDir </> filePrefix ++ suffix

    luts :: LUTs
    luts@LUTs{..} = simplify $ repeatedly insert msps empty

    rangeIds :: RangeIds
    rangeIds@RangeIds{..} = assignRangeIds luts

data LUTs = LUTs {
      -- | Unique ID assigned to each sample range
      sampleIds :: Map Sample SampleId

      -- | Split (non-overlapping) selector ranges
    , splitSelector :: Split Int (Set SampleId)

      -- | Split key ranges
    , splitKey :: Split MidiNote (Set SampleId)

      -- | Split velocity ranges
    , splitVelocity :: Split Int (Set SampleId)

      -- | Split articulation ranges
    , splitChain :: Split Int (Set SampleId)

      -- | Unique selector ranges in the original data (possibly overlapping)
    , uniqueSelector :: Set (Interval Int)

      -- | Unique key ranges in the original data (possibly overlapping)
    , uniqueKey :: Set (Interval MidiNote)

      -- | Unique velocity ranges in the original data (possibly overlapping)
    , uniqueVelocity :: Set (Interval Int)

      -- | Unique chain ranges in the original data (possibly overlapping)
    , uniqueChain :: Set (Name, Interval Int)
    }
  deriving (Show)

newtype SampleId = SampleId Int
  deriving newtype (Show, Eq, Ord)

empty :: LUTs
empty = LUTs {
      sampleIds      = Map.empty
    , splitSelector  = Split.empty
    , splitKey       = Split.empty
    , splitVelocity  = Split.empty
    , splitChain     = Split.empty
    , uniqueSelector = Set.empty
    , uniqueKey      = Set.empty
    , uniqueVelocity = Set.empty
    , uniqueChain    = Set.empty
    }

insert :: MSP -> LUTs -> LUTs
insert MSP{..} LUTs{..} = LUTs{
      sampleIds      = Map.insert sample sampleId sampleIds
    , splitSelector  = modifySplit selector   splitSelector
    , splitKey       = modifySplit key        splitKey
    , splitVelocity  = modifySplit velocity   splitVelocity
    , splitChain     = modifySplit chainRange splitChain
    , uniqueSelector = Set.insert selector            uniqueSelector
    , uniqueKey      = Set.insert key                 uniqueKey
    , uniqueVelocity = Set.insert velocity            uniqueVelocity
    , uniqueChain    = Set.insert (chain, chainRange) uniqueChain
    }
  where
    modifySplit ::
         (Ord v, Enum v)
      => Interval v
      -> Split v (Set SampleId) -> Split v (Set SampleId)
    modifySplit = Split.modify Set.empty (Set.insert sampleId)

    -- Reserve ID 0 for "not found"
    sampleId :: SampleId
    sampleId =
        case Map.lookup sample sampleIds of
          Nothing -> SampleId $ (Map.size sampleIds + 1)
          Just id -> id

{-------------------------------------------------------------------------------
  Assign IDs to each range
-------------------------------------------------------------------------------}

newtype RangeIdSelector = RangeIdSelector Int deriving newtype (Show, Num)
newtype RangeIdKey      = RangeIdKey      Int deriving newtype (Show, Num)
newtype RangeIdVelocity = RangeIdVelocity Int deriving newtype (Show, Num)
newtype RangeIdChain    = RangeIdChain    Int deriving newtype (Show, Num)

-- | Range IDs
--
-- Once all data is processed, we assign a unique ID to each /split/ range.
-- It is this range ID that the LUTs work with.
--
-- Range IDs are assigned starting from 1, reserving 0 for "no matching range".
data RangeIds = RangeIds {
      -- | Unique ID assigned to each selector range
      rangesSelector :: Map (Interval Int) RangeIdSelector

      -- | Unique ID assigned to each key range
    , rangesKey :: Map (Interval MidiNote) RangeIdKey

      -- | Unique ID assigned to each velocity range
    , rangesVelocity :: Map (Interval Int) RangeIdVelocity

      -- | Unique ID assigned to each chain range
    , rangesChain :: Map (Interval Int) RangeIdChain
    }
  deriving (Show)

assignRangeIds :: LUTs -> RangeIds
assignRangeIds LUTs{..} = RangeIds {
      rangesSelector = numberRanges splitSelector
    , rangesKey      = numberRanges splitKey
    , rangesVelocity = numberRanges splitVelocity
    , rangesChain    = numberRanges splitChain
    }
  where
    numberRanges :: (Ord v, Num b) => Split v a -> Map (Interval v) b
    numberRanges xs =
        Map.fromList $
          zip (Set.toList (Split.keysSet xs)) (map fromInteger [1..])

{-------------------------------------------------------------------------------
  Simplification
-------------------------------------------------------------------------------}

simplify :: LUTs -> LUTs
simplify = mergeIdentical

mergeIdentical :: LUTs -> LUTs
mergeIdentical LUTs{..} = LUTs{
      splitSelector = merge splitSelector
    , splitKey      = merge splitKey
    , splitVelocity = merge splitVelocity
    , splitChain    = merge splitChain
    , ..
    }
  where
    merge :: (Ord v, Eq a) => Split v a -> Split v a
    merge = Split.mergeAdjacentIf $ \(i, xs) (j, ys) -> do
            guard (xs == ys)
            return (I.union i j, xs)

{-------------------------------------------------------------------------------
  Checking for overlaps
-------------------------------------------------------------------------------}

data Overlap a = Overlap {
      overlapSelector     :: Interval Int
    , overlapKey          :: Interval MidiNote
    , overlapVelocity     :: Interval Int
    , overlapArticulation :: Interval Int
    , overlapValues       :: [a] -- ^ At least two values
    }
  deriving (Show, Functor)

overlaps :: LUTs -> [Overlap SampleId]
overlaps LUTs{..} = [
        overlap
      | (selector, samplesForSelector) <- Split.toList splitSelector
      , (key,      samplesForKey)      <- Split.toList splitKey
      , (velocity, samplesForVelocity) <- Split.toList splitVelocity
      , (chain,    samplesForChain)    <- Split.toList splitChain
      , let intersection = foldr1 Set.intersection [
                samplesForSelector
              , samplesForKey
              , samplesForVelocity
              , samplesForChain
              ]
      , Set.size intersection > 1
      , let overlap = Overlap {
                overlapSelector     = selector
              , overlapKey          = key
              , overlapVelocity     = velocity
              , overlapArticulation = chain
              , overlapValues       = Set.toList intersection
              }
      ]

{-------------------------------------------------------------------------------
  Generate LUTs
-------------------------------------------------------------------------------}

toRangeId :: forall a id.
     (Ord a, Enum a, Show id, Num id)
  => Map (Interval a) id -> String
toRangeId ids = unwords ("table" : map show ranges)
  where
    ranges :: [id]
    ranges = [
          case select x of
            []  -> 0 -- Not found
            [i] -> i
            _   -> error "toRangeId: Multiple matches"
        | x <- map toEnum [0 .. 127]
        ]

    select :: a -> [id]
    select x = map snd $ filter ((`I.contains` x) . fst) (Map.toList ids)

matchingRangeId ::
     (Show id, Ord a)
  => Map (Interval a) id
  -> Split a (Set SampleId)
  -> String
matchingRangeId ids ranges = unlines [
      show id ++ ", " ++ unwords (map show $ Set.toList samples) ++ ";"
    | (i, id) <- Map.toList ids
    , Just samples <- [Split.lookup i ranges]
    ]

-- | LUT for the samples themselves
--
-- We output these as
--
-- > <sample ID, <start> <end> <volume> <sample>;
--
-- This ordering is most useful in the @ks-sampler-core@:
--
-- 1. Set volume
-- 2. Set @play~@ sample buffer
-- 3. Issue play command
sampleLUT :: Double -> Map Sample SampleId -> String
sampleLUT sampleRate =
    unlines . map (uncurry aux) . sortOn fst . map swap . Map.toList
  where
    aux :: SampleId -> Sample -> String
    aux sid Sample{
                file   = Name n
              , range  = (SampleStart fr, SampleEnd to)
              , volume = Volume v
              } = concat [
          show sid
        , ", "
        , show (fromIntegral fr / sampleRate * 1_000)
        , " "
        , show (fromIntegral to / sampleRate * 1_000)
        , " "
        , fileToSymbol n
        , " "
        , show v
        , ";"
        ]

fileToSymbol :: String -> String
fileToSymbol =
     map (\x -> if x `elem` " -" then '_' else x)
   . takeWhile (/= '.') -- drop the extension
   . map toLower
