module TrackSelected.State (
    State(..)
  , defaultState
  ) where

import Prelude
import Data.Maybe (Maybe(..))

import MaxForLive.LiveAPI (Id, Device)

-- | Patcher state
-- |
-- | Although the details are a bit different, we follow the strategy outlined
-- | in http://edsko.net/2020/12/26/trichords-part1/
data State = State {
      -- | ID of our device ('Nothing' if not yet known)
      -- |
      -- | When we initialize the device, we set up a `live.path` object to
      -- | monitor the device at our path. When we get a notification from this
      -- | monitor, we compare the ID we get with our own ID. If it is changed,
      -- | that means the device was moved and we should re-initialize it.
      ourId :: Maybe (Id Device)
    }

derive instance eqState :: Eq State

instance showState :: Show State where
  show (State state) = show state

defaultState :: State
defaultState = State {
      ourId: Nothing
    }
