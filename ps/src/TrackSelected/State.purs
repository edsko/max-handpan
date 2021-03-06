module TrackSelected.State (
    State
  , init
  , isSelected
  ) where

import Prelude
import Data.Maybe (Maybe(..))

import MaxForLive.LiveAPI (Id, Device, Track)

-- | Patcher state
-- |
-- | Although the details are a bit different, we follow the strategy outlined
-- | in http://edsko.net/2020/12/26/trichords-part1/
type State = {
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

      -- | Preview mode
      -- |
      -- | We assume this starts at `True` (i.e., the device within the Max
      -- | for Live window is the active one, not the one on the Live track).
      -- |
      -- | https://docs.cycling74.com/max8/vignettes/live_preview
      --
      -- We monitor preview mode, because when it is disabled and then
      -- re-enabled, the device within the Max for Live window should
      -- be re-initialized.
      --
      -- When the device is loaded within Live rather than within Max, the
      -- "Preview Mode" concept does not apply, and we will not get
      -- notifications about it changing. This means we will consider the
      -- device to be in "preview mode" always; this is important, because
      -- we consider the device to be selected only when in preview mode.
    , preview :: Boolean
    }

-- | Initial state
init :: State
init = {
      ourId: Nothing
    , parent: Nothing
    , selected: Nothing
    , enabled: true
    , preview: true
    }

-- | Is the device selected (and enabled?)
isSelected :: State -> Boolean
isSelected { enabled, parent, selected, preview } =
    enabled && parent == selected && preview
