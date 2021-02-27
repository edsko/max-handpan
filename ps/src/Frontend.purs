module Frontend (main) where

import Prelude
import Data.Maybe (Maybe(..))
import Effect (Effect)

import MaxForLive.Global (
    setInlets
  , setOutlets
  , outlet
  , postLn
  )
import MaxForLive.Handlers (
    setHandler
  )
import MaxForLive.Push (Push(..))
import MaxForLive.Push as Push

main :: Effect Unit
main = do
    setInlets  1
    setOutlets 2

    mPush <- Push.new
    case mPush of
      Nothing ->
        postLn "No push found"
      Just push -> do
        postLn "Found the Push. Setting up handlers"
        registerHandlers push

registerHandlers :: Push -> Effect Unit
registerHandlers (Push push) = do
    setHandler { inlet: 0, msg: "grab", handler: push.grabButtonMatrix }
    setHandler { inlet: 0, msg: "release", handler: push.releaseButtonMatrix }
    push.setButtonMatrixColor { col: 1, row: 2 } 10
    outlet 1 push.buttonMatrixId
