module TrackSelected.State (
    State(..)
  , init
  , setDevice
  , setEnabled
  , setSelected
  , isSelected
  ) where

import Prelude
import Data.Maybe (Maybe(..))

import MaxForLive.LiveAPI (Id, Device, Track)

-- | Patcher state
-- |
-- | Although the details are a bit different, we follow the strategy outlined
-- | in http://edsko.net/2020/12/26/trichords-part1/
data State = State {
      -- | ID of our device (`Nothing` if not yet known)
      -- |
      -- | When we initialize the device, we set up a `live.path` object to
      -- | monitor the device at our path. When we get a notification from this
      -- | monitor, we compare the ID we get with our own ID. If it is changed,
      -- | that means the device was moved and we should re-initialize it.
      ourId :: Maybe (Id Device)

      -- | Our parent track (`Nothing` if not yet known)
      --
      -- If this equals the `selected` track and we are `enabled`, we consider
      -- the device to be "selected"
    , parent :: Maybe (Id Track)

      -- | ID of the selected track (`Nothing` if not yet known)
    , selected :: Maybe (Id Track)

      -- | Is the device enabled?
      -- |
      -- | We assume the device is enabled when it is loaded.
    , enabled :: Boolean
    }

instance showState :: Show State where
  show (State state) = show state

-- | Initial state
init :: State
init = State {
      ourId: Nothing
    , parent: Nothing
    , selected: Nothing
    , enabled: true
    }

setDevice :: { ourId :: Id Device, parent :: Id Track } -> State -> State
setDevice { ourId, parent } (State state) = State $ state {
      ourId  = Just ourId
    , parent = Just parent
    }

setEnabled :: Boolean -> State -> State
setEnabled enabled (State state) = State $ state { enabled = enabled }

setSelected :: Id Track -> State -> State
setSelected selected (State state) = State $ state { selected = Just selected }

-- | Is the device selected (and enabled?)
isSelected :: State -> Boolean
isSelected (State { enabled, parent, selected }) =
    enabled && parent == selected
