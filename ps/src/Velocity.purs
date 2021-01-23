module Velocity (main) where

import Prelude

import Data.Maybe (Maybe(..))

import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref

import MaxForLive.Global (
    jsArg
  , numJsArgs
  , outlet
  , postLn
  , setInlets
  , setOutlets
  )
import MaxForLive.Handlers (registerHandler)
import MaxForLive.Objects (LiveDial(..))
import MaxForLive.Objects as Obj
import MaxForLive.Patcher (Maxobj, patcher)
import MaxForLive.Patcher as Patcher

main :: Effect Unit
main = do
    postLn $ "We have " <> show numJsArgs <> " arguments"
    postLn $ "Argument 1 is " <> show (jsArg 1 :: Int)
    postLn $ "Argument 2 is " <> show (jsArg 2 :: String)

    postLn $ "Our patcher is " <> Patcher.filepath patcher

    toggleRef <- Ref.new Nothing

    setInlets  1
    setOutlets 1

    registerHandler "bang"    $ bang toggleRef
    registerHandler "say"     $ say
    registerHandler "sum"     $ sum
    registerHandler "msg_int" $ say

bang :: Ref (Maybe (Maxobj LiveDial)) -> Effect Unit
bang ref = do
    mOld <- Ref.read ref
    case mOld of
      Nothing -> do
        obj <- Patcher.newDefault patcher {left: 122, top: 90} $ LiveDial {
                   liveDialType: Obj.liveDialTypeInt
                 }
        Patcher.connect patcher {
            fromObject: obj
          , outlet: 0
          , toObject: Patcher.box
          , inlet: 0
          }
        Ref.write (Just obj) ref
      Just old -> do
        Patcher.remove patcher old
        Ref.write Nothing ref

say :: Int -> Effect Unit
say n = postLn $ "PureScript says int " <> show n

sum :: Int -> Int -> Effect Unit
sum n m = outlet 0 (n + m)
