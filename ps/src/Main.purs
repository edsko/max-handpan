module Main (
    sayHi
  ) where

import Prelude
import Effect (Effect)

import Live.Console (post)

sayHi :: Effect Unit
sayHi = post "hi from PureScript"
