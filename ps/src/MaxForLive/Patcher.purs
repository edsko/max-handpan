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
  , Maxobj
  , newDefault
  , remove
  , connect
  , box
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
foreign import data Maxobj :: Type -> Type

-- | The `Maxobj` corresponding to the `js` object
--
-- There does not seem to be a documentation entry for this; the tutorial
-- mentions it at https://docs.cycling74.com/max8/tutorials/javascriptchapter02,
-- but it's not listed in https://docs.cycling74.com/max8/vignettes/jsglobal.
--
-- _Not_ the same as
-- https://docs.cycling74.com/max8/vignettes/jspatcherobject#box.
foreign import box :: Maxobj Patcher

data Toggle = Toggle

-- | Add a new object to the patcher
--
-- See https://docs.cycling74.com/max8/vignettes/jspatcherobject#newdefault
foreign import newDefaultImpl ::
    forall a. EffectFn2 Patcher (NewDefaultArgs a) (Maxobj a)

-- | Removes the object (a Maxobj passed as an argument) from a patcher
--
-- See https://docs.cycling74.com/max8/vignettes/jspatcherobject#remove
foreign import remove :: forall a. Patcher -> Maxobj a -> Effect Unit

-- | Connects two objects
--
-- https://docs.cycling74.com/max8/vignettes/jspatcherobject#connect
foreign import connect ::
     forall a b.
     Patcher
  -> { fromObject :: Maxobj a
     , inlet      :: Int
     , toObject   :: Maxobj b
     , outlet     :: Int
     }
  -> Effect Unit

newDefault :: forall a.
     MkNew a
  => Patcher
  -> {left :: Int, top :: Int}
  -> a -> Effect (Maxobj a)
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
