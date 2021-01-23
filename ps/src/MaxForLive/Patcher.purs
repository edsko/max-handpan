-- | Access to the patcher itself
-- |
-- | See https://docs.cycling74.com/max8/vignettes/jspatcherobject
-- |
-- | This module is intended for qualified import.
module MaxForLive.Patcher (
    Patcher
  , patcher
  , filepath
    -- | Dynamic patcher manipulation
  , MaxObj
  , newDefault
  , remove
    -- | Specific kinds of Max objects
  , Toggle(..)
    -- | FFI: Create
  , class MkNew
  , mkNewDefaultArgs
  , NewDefaultArgs
  ) where

import Prelude

import Data.Function.Uncurried (Fn2, runFn2)
import Effect (Effect)
import Effect.Uncurried (EffectFn2, runEffectFn2)

-- | The type of the patcher
foreign import data Patcher :: Type

-- | Our own patcher
-- |
-- | https://docs.cycling74.com/max8/vignettes/jsglobal#patcher
foreign import patcher :: Patcher

-- | The patcher’s file path on disk
foreign import filepath :: Patcher -> String

{-------------------------------------------------------------------------------
  Dynamically manipulating the patcher

  See also https://docs.cycling74.com/max8/tutorials/javascriptchapter02
-------------------------------------------------------------------------------}

-- | Representation of a Max object in a patcher.
--
-- The phantom type argument that tells us the corresponding PureScript type.
--
-- https://docs.cycling74.com/max8/vignettes/jsmaxobj
foreign import data MaxObj :: Type -> Type

data Toggle = Toggle

-- | Add a new object to the patcher
--
-- See https://docs.cycling74.com/max8/vignettes/jspatcherobject#newdefault
foreign import newDefaultImpl ::
    forall a. EffectFn2 Patcher (NewDefaultArgs a) (MaxObj a)

-- | Removes the object (a Maxobj passed as an argument) from a patcher
--
-- See https://docs.cycling74.com/max8/vignettes/jspatcherobject#remove
foreign import remove :: forall a. Patcher -> MaxObj a -> Effect Unit

newDefault :: forall a.
     MkNew a
  => Patcher
  -> {left :: Int, top :: Int}
  -> a -> Effect (MaxObj a)
newDefault p {left, top} a =
    runEffectFn2 newDefaultImpl p $ mkNewDefaultArgs a left top

{-------------------------------------------------------------------------------
  FFI: Construct untyped arguments for object creation
-------------------------------------------------------------------------------}

-- | Arguments needed to initialize a new object
foreign import data NewDefaultArgs :: Type -> Type

foreign import mkNewDefaultArgsToggle :: Fn2 Int Int (NewDefaultArgs Toggle)

class MkNew a where
  mkNewDefaultArgs :: a -> Int -> Int -> NewDefaultArgs a

instance mkNewToggle :: MkNew Toggle where
  mkNewDefaultArgs Toggle = runFn2 mkNewDefaultArgsToggle
