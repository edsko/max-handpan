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
        mkHandler "bang" bang
      , mkHandler "say"  say
      , mkHandler "sum"  sum
      ]

bang :: Effect Unit
bang = postLn "PureScript says bang"

say :: Int -> Effect Unit
say n = postLn $ "PureScript says int " <> show n

sum :: Int -> Int -> Effect Unit
sum n m = postLn $ "PostScript sum: " <> show n <> " + " <> show m
