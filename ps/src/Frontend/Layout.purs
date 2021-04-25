module Frontend.Layout (
    Layout(..)
  , defaultLayout
  ) where

-- import Prelude

import MaxForLive.Push (Button)

data Layout = Layout {
      tonefields :: Array Button -- ^ One per tonefield
    , ghostnotes :: Array Button -- ^ One per ghost note
    , slaps      :: Array Button -- ^ One per slap position
    , taks       :: Array Button -- ^ One per tak position (left/right of the doum)
    , doum       :: Array Button -- ^ Multiple buttons mapping to doum
    , bass       :: Array Button -- ^ Multiple buttons mapping to the bass
    }

defaultLayout :: Layout
defaultLayout = Layout {
      tonefields: [
          {col: 4, row: 7}
        , {col: 0, row: 5}
        , {col: 7, row: 5}
        , {col: 0, row: 3}
        , {col: 7, row: 3}
        , {col: 0, row: 1}
        , {col: 7, row: 1}
        , {col: 3, row: 0}
        , {col: 4, row: 0}
        ]

    , ghostnotes: [
          {col: 4, row: 6}
        , {col: 1, row: 5}
        , {col: 6, row: 5}
        , {col: 1, row: 3}
        , {col: 6, row: 3}
        , {col: 1, row: 1}
        , {col: 6, row: 1}
        , {col: 3, row: 1}
        , {col: 4, row: 1}
        ]

    , slaps: [
          {col: 0, row: 6}
        , {col: 7, row: 6}
        ]

    , taks: [
          {col: 2, row: 4}
        , {col: 5, row: 4}
        ]

    , doum: [
          {col: 3, row: 4}
        , {col: 4, row: 4}
        ]

    , bass: [
          {col: 0, row: 7}
        , {col: 1, row: 7}
        , {col: 6, row: 7}
        , {col: 7, row: 7}
        ]
    }
