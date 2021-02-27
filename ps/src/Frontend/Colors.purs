module Frontend.Colors (
    Colors(..)
  , defaultColors
  ) where

import MaxForLive.Push (Color)

data Colors = Colors {
      bass      :: Color
    , doum      :: Color
    , ghostnote :: Color
    , slap      :: Color
    , tak       :: Color
    , tonefield :: Color
    }

defaultColors :: Colors
defaultColors = Colors {
      bass: 74
    , doum: 3
    , ghostnote: 20
    , slap: 68
    , tak: 6
    , tonefield: 16
    }
