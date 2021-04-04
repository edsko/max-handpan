module XML.TypeDriven.AttrNode where

import Data.String (fromString)
import Data.Typeable
import GHC.TypeLits

import XML.Parser qualified as P
import XML.TypeDriven.API

-- | Node with a single attribute of the given type
--
-- This doesn't really add any new expressivity over 'Node', but it captures
-- a common case and avoids unnecessary clutter in the AST.
--
-- Can be used directly, but primarily intended for use with deriving-via:
--
-- > newtype Name = Name Text
-- >   deriving stock (Show, Data)
-- >   deriving Parse via AttrNode "Name" "Value" Text
newtype AttrNode (tag :: Symbol) (attr :: Symbol) a = AttrNode a

instance (
      KnownSymbol tag
    , KnownSymbol attr
    , Typeable a
    , ParseAttr a
    ) => Parse (AttrNode tag attr a) where
  parse =
      P.tag'
        (symbolVal (Proxy @tag))
        (parseAttr (fromString (symbolVal (Proxy @attr))))
        (return . AttrNode)
