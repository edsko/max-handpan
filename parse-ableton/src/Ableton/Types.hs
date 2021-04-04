module Ableton.Types (
    -- * MIDI notes
    MidiNote(..)
  , Octave
  , Note(..)
  ) where

import Data.Data
import Data.Function (on)

import XML.Aux
import XML.TypeDriven

{-------------------------------------------------------------------------------
  MIDI notes
-------------------------------------------------------------------------------}

data MidiNote = MidiNote Note Octave
  deriving (Eq, Data)

-- | Custom (unlawful) 'Show' instance
--
-- Unlawful but more convenient
instance Show MidiNote where
  show (MidiNote n o) = show n ++ show o

-- | Ord instance makes sure notes from lower octaves are considered "smaller"
instance Ord MidiNote where
  compare = compare `on` (\(MidiNote n o) -> (o, n))

instance Enum MidiNote where
  toEnum n = MidiNote (toEnum note) (scale - 2)
    where
      (scale, note) = n `divMod` 12

  fromEnum (MidiNote n o) = (o + 2) * 12 + fromEnum n

-- | Octaves (as Ableton parses them)
--
-- We will report MIDI note @69@ as octave 3, because that's what Ableton does.
type Octave = Int

data Note =
    C
  | Db -- == C#
  | D
  | Eb -- == D#
  | E
  | F
  | Gb -- == F#
  | G
  | Ab -- == G#
  | A
  | Bb -- == A#
  | B
  deriving (Show, Eq, Ord, Enum, Data)

instance ParseAttr MidiNote where
  parseAttr = attrEnum
