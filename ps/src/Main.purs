module Main (main) where

import Prelude
import Effect (Effect)

import MaxForLive.Global (
    jsArg
  , numJsArgs
  , outlet
  , postLn
  , setInlets
  , setOutlets
  )
import MaxForLive.Handlers (setHandlers, mkHandler)

main :: Effect Unit
main = do
    postLn $ "We have " <> show numJsArgs <> " arguments"
    postLn $ "Argument 1 is " <> show (jsArg 1 :: Int)
    postLn $ "Argument 2 is " <> show (jsArg 2 :: String)

    setInlets  1
    setOutlets 1

    setHandlers [
        mkHandler "bang"    bang
      , mkHandler "say"     say
      , mkHandler "sum"     sum
      , mkHandler "msg_int" say
      ]

bang :: Effect Unit
bang = postLn "PureScript says bang"

say :: Int -> Effect Unit
say n = postLn $ "PureScript says int " <> show n

sum :: Int -> Int -> Effect Unit
sum n m = outlet 0 (n + m)
