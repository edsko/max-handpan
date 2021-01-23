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
import MaxForLive.Handlers (registerHandler)

main :: Effect Unit
main = do
    postLn $ "We have " <> show numJsArgs <> " arguments"
    postLn $ "Argument 1 is " <> show (jsArg 1 :: Int)
    postLn $ "Argument 2 is " <> show (jsArg 2 :: String)

    setInlets  1
    setOutlets 1

    registerHandler "bang"    bang
    registerHandler "say"     say
    registerHandler "sum"     sum
    registerHandler "msg_int" say

bang :: Effect Unit
bang = postLn "PureScript says bang"

say :: Int -> Effect Unit
say n = postLn $ "PureScript says int " <> show n

sum :: Int -> Int -> Effect Unit
sum n m = outlet 0 (n + m)
