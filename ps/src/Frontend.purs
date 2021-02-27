module Frontend (main) where

import Prelude
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Effect (Effect)

import MaxForLive.Global (
    setInlets
  , setOutlets
  , outlet
  , postLn
  )
import MaxForLive.Handlers (setHandler)
import MaxForLive.Message (Message(..))
import MaxForLive.Push (Push(..))
import MaxForLive.Push as Push

import Frontend.Layout (Layout(..), defaultLayout)
import Frontend.Colors (Colors(..), defaultColors)

main :: Effect Unit
main = do
    setInlets  1
    setOutlets 1

    -- TODO: We should initialize the push on device init (can't use the
    -- LiveAPI before that)

    mPush <- Push.new
    case mPush of
      Nothing ->
        postLn "No push found"
      Just push -> do
        postLn "Found the Push. Setting up handlers"
        setup push

setup :: Push -> Effect Unit
setup (Push push) = do
    setHandler { inlet: 0, msg: "grab", handler: push.grabButtonMatrix }
    setHandler { inlet: 0, msg: "release", handler: push.releaseButtonMatrix }

    for_ layout.tonefields $ \button -> do
      push.setButtonMatrixColor button colors.tonefield
    for_ layout.ghostnotes $ \button -> do
      push.setButtonMatrixColor button colors.ghostnote
    for_ layout.slaps $ \button -> do
      push.setButtonMatrixColor button colors.slap

    for_ layout.doum $ \button -> do
      push.setButtonMatrixColor button colors.doum
    for_ layout.tak $ \button -> do
      push.setButtonMatrixColor button colors.tak
    for_ layout.bass $ \button -> do
      push.setButtonMatrixColor button colors.bass

    outlet 0 $ Message {
        messageName: "buttonMatrixId"
      , messagePayload: push.buttonMatrixId
      }
  where
    Layout layout = defaultLayout
    Colors colors = defaultColors
