module Main (
    main
  ) where

import Prelude
import Effect (Effect)

import Live.Global (setInlets, setOutlets, postLn)
import Live.Handlers (setHandlers, mkHandler)

main :: Effect Unit
main = do
    setInlets  1
    setOutlets 1

    setHandlers [
        { message: "bang", handler: mkHandler bang }
      ]

bang :: Effect Unit
bang = postLn "Bang from PureScript"
