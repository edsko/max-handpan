module Frontend (main) where

import Prelude
import Data.Foldable (for_)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Maybe (Maybe(..))
import Effect (Effect)

import MaxForLive.Global (
    setInlets
  , setOutlets
  , outlet
  , postLn
  )
import MaxForLive.Handlers (setHandler)
import MaxForLive.Message (Message(..), Bang(..))
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

    outlet 0 $ Message {
        messageName: "buttonMatrixId"
      , messagePayload: push.buttonMatrixId
      }
    outlet 0 $ Message {
        messageName: "reset"
      , messagePayload: Bang
      }

    forWithIndex_ layout.tonefields $ \ix button -> do
      push.setButtonMatrixColor button colors.tonefield
      outlet 0 $ Message {
          messageName: "setNote"
        , messagePayload: [ button.col * 8 + button.row , 48 + ix ]
        }
      outlet 0 $ Message {
          messageName: "setVelocity"
        , messagePayload: [ button.col * 8 + button.row , 1 ]
        }
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

  where
    Layout layout = defaultLayout
    Colors colors = defaultColors
