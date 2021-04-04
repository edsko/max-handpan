{-# LANGUAGE OverloadedStrings #-}

-- | Auxiliary functions that are missing in "Text.XML.Stream.Parse"
module XML.Aux (
    -- * Attribute parsing
    requireAttrCI
  , attrRead
  , attrReadMaybe
  , attrEnum
  , attrEnumMaybe
    -- * Re-exports
  , contentsToText
  ) where

import Control.Monad (guard)
import Data.Function (on)
import Data.Text (Text)
import Data.Text qualified as T
import Data.XML.Types
import Text.Read (readMaybe)

import Text.XML.Stream.Parse

{-------------------------------------------------------------------------------
  Attribute parsing
-------------------------------------------------------------------------------}

-- | Case insensitive version of 'requireAttr'
requireAttrCI :: Name -> AttrParser Text
requireAttrCI = \n ->
      force ("Missing attribute: " ++ show n)
    $ optionalAttrRaw
    $ \(n', c) -> guard (sameName n n') >> return (contentsToText c)
  where
    -- Adopted from the 'Eq' instance of 'Name'
    sameName :: Name -> Name -> Bool
    sameName = (==) `on` ( \x -> ( nameNamespace x
                                 , T.toLower (nameLocalName x)
                                 ) )

-- | Attribute parser for anything that implements 'Read'
attrReadMaybe :: Read a => Name -> AttrParser (Maybe a)
attrReadMaybe name = readMaybe . T.unpack <$> requireAttrCI name

-- | Variation on 'attrReadMaybe' that fails on 'Nothing'
attrRead :: Read a => Name -> AttrParser a
attrRead name =
    force ("Could not parse value of attribute " ++ show name) $
      attrReadMaybe name

-- | Attribute parser for anything that implements 'Enum'
attrEnumMaybe :: Enum a => Name -> AttrParser (Maybe a)
attrEnumMaybe name = fmap toEnum <$> attrReadMaybe name

-- | Variation on 'attrEnumMaybe' that fails on 'Nothing'
attrEnum :: Enum a => Name -> AttrParser a
attrEnum name = toEnum <$> attrRead name

{-------------------------------------------------------------------------------
  "Re-exports" (defined in "Text.XML.Stream.Parse" but not exported there)

  These are just copy and paste.
-------------------------------------------------------------------------------}

contentsToText :: [Content] -> Text
contentsToText = T.concat . map toText where
  toText (ContentText t)   = t
  toText (ContentEntity e) = T.concat ["&", e, ";"]
