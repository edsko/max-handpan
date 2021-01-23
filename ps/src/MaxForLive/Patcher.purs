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
    -- | FFI: Create
  , class MkNew
  , mkNewDefaultArgs
  , mkNewAttrs
  ) where

import Prelude

import Effect (Effect)
import Effect.Uncurried (EffectFn2, runEffectFn2)

import MaxForLive.Conversions (MaxValue)

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

-- | Add a new object to the patcher
--
-- See https://docs.cycling74.com/max8/vignettes/jspatcherobject#newdefault
foreign import newDefaultImpl ::
    forall a. EffectFn2 Patcher (Array MaxValue) (Maxobj a)

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

-- | Set attributes
--
-- https://docs.cycling74.com/max8/vignettes/jsmaxobj#setattr
foreign import setAttr ::
     forall a.
     EffectFn2
       (Maxobj a)
       (Array {attr :: String, value :: MaxValue})
       Unit

newDefault :: forall a.
     MkNew a
  => Patcher
  -> {left :: Int, top :: Int}
  -> a -> Effect (Maxobj a)
newDefault p coords a = do
    obj <- runEffectFn2 newDefaultImpl p $ mkNewDefaultArgs a coords
    runEffectFn2 setAttr obj $ mkNewAttrs a
    pure obj

{-------------------------------------------------------------------------------
  FFI: Construct untyped arguments for object creation
-------------------------------------------------------------------------------}

class MkNew a where
  -- | Arguments that should be provided to `newdefault`
  mkNewDefaultArgs :: a -> {left :: Int, top :: Int} -> Array MaxValue

  -- | Attributes that should be set after the object has been created
  --
  -- For an example, see
  --
  -- * https://cycling74.com/tutorials/building-a-synthesizer-editor-with-javascript-part-2/
  -- * https://cycling74.com/forums/can't-create-live-object-withi-javascript
  mkNewAttrs :: a -> Array {attr :: String, value :: MaxValue}
