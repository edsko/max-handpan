module ScaleSelector (main) where

import Prelude
import Effect (Effect)

import MaxForLive.Global (
    postLn
  , setInletAssist
  , setInlets
  , setOutlets
  )
import MaxForLive.Handlers (setHandler)

main :: Effect Unit
main = do
    setInlets  3
    setOutlets 1

    setInletAssist 0 "Scale"
    setInletAssist 1 "Root"
    setInletAssist 2 "Doum"

    setHandler { inlet: 0, msg: "msg_int", handler: setScale }
    setHandler { inlet: 1, msg: "msg_int", handler: setRoot  }
    setHandler { inlet: 2, msg: "msg_int", handler: setDoum  }

setScale :: Int -> Effect Unit
setScale scale = postLn $ "setScale " <> show scale

setRoot :: Int -> Effect Unit
setRoot root = postLn $ "setRoot " <> show root

setDoum :: Int -> Effect Unit
setDoum doum = postLn $ "setDoum " <> show doum
