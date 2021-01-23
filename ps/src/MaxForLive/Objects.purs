-- | MaxForLive objects (toggle, dial, etc.)
module MaxForLive.Objects (
    -- | `toggle`
    Toggle(..)
    -- | `live.dial`
  , LiveDial(..)
  , LiveDialType
  , liveDialTypeFloat
  , liveDialTypeInt
  , liveDialTypeEnum
  ) where

import MaxForLive.Conversions (MaxValue, toMax)
import MaxForLive.Patcher (class MkNew)

{-------------------------------------------------------------------------------
  `toggle`
-------------------------------------------------------------------------------}

-- | `toggle`
-- |
-- | https://docs.cycling74.com/max8/refpages/toggle
data Toggle = Toggle

instance mkNewToggle :: MkNew Toggle where
  mkNewDefaultArgs Toggle = defaultMkNewDefaultArgs "toggle"
  mkNewAttrs       Toggle = []

{-------------------------------------------------------------------------------
  `live.dial`
-------------------------------------------------------------------------------}

-- | `live.dial`
-- |
-- | https://docs.cycling74.com/max8/refpages/live.dial
--
-- Implemention note: to figure out the parameters, the "copy and paste"
-- trick from https://cycling74.com/forums/what-is-this-message-_parameter_-about-max-for-live-ui-objects-2
-- seems to work quite well.
data LiveDial = LiveDial {
      liveDialType :: LiveDialType
    }

newtype LiveDialType = LiveDialType Int

liveDialTypeFloat :: LiveDialType
liveDialTypeFloat = LiveDialType 0

liveDialTypeInt :: LiveDialType
liveDialTypeInt = LiveDialType 1

liveDialTypeEnum :: LiveDialType
liveDialTypeEnum = LiveDialType 2

instance mkNewLiveDial :: MkNew LiveDial where
  mkNewDefaultArgs (LiveDial _) = defaultMkNewDefaultArgs "live.dial"

  mkNewAttrs (LiveDial {liveDialType: LiveDialType typ}) = [
        {attr: "_parameter_type", value: toMax typ}
      ]

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

-- | For many objects, the arguments for `newdefault` are all the same:
-- the left and top coordinates and then a string indicating the type of
-- the object.
defaultMkNewDefaultArgs :: String -> {left :: Int, top :: Int} -> Array MaxValue
defaultMkNewDefaultArgs label {left, top} = [
      toMax left
    , toMax top
    , toMax label
    ]
