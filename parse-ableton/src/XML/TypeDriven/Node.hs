-- | Type driven XML parsing
module XML.TypeDriven.Node (
    -- * Nodes
    Node(..)
  , Attrs
  , Required
  , Optional
    -- * Parsing
  , Parse(..)
  , ParseNode(..)
    -- ** Convenience re-exports
  , Proxy(..)
  ) where

import Data.Data
import Data.Either (partitionEithers)
import Data.Kind (Type)
import Data.String (fromString)
import Generics.SOP

import Text.XML.Stream.Parse

import Util.SOP
import XML.Parser (Parser)
import XML.Parser qualified as P
import XML.TypeDriven.API

{-------------------------------------------------------------------------------
  Nodes
-------------------------------------------------------------------------------}

data family Attrs    (a :: dom) :: Type
data family Required (a :: dom) :: Type
data family Optional (a :: dom) :: Type

data Node (a :: dom) = Node {
      attrs    :: Attrs     a
    , required :: Required  a
    , optional :: [Optional a]
    }

deriving instance (
    Show (Attrs    a)
  , Show (Required a)
  , Show (Optional a)
  ) => Show (Node a)

deriving instance (
    Typeable a
  , Typeable dom
  , Data (Attrs    a)
  , Data (Required a)
  , Data (Optional a)
  ) => Data (Node (a :: dom))

{-------------------------------------------------------------------------------
  Default parser for nodes
-------------------------------------------------------------------------------}

type family FromNode dom (node :: Type) :: dom where
  FromNode dom (Node (x :: dom)) = x

-- | Intended for use in @deriving via@
newtype ParseNode a = ParseNode a

instance (
      node ~ Node (FromNode dom node)
      -- For the name of the XML tag
    , Typeable (FromNode dom node)
      -- For error messages
    , Typeable dom
      -- Attributes
    , HasDatatypeInfo (Attrs (FromNode dom node))
    , SingletonSatisfying (All ParseAttr) (Code (Attrs (FromNode dom node)))
      -- Required children
    , HasDatatypeInfo (Required (FromNode dom node))
    , SingletonSatisfying (All Parse) (Code (Required (FromNode dom node)))
      -- Optional children
    , Generic (Optional (FromNode dom node))
    , All (SingletonSatisfying Parse) (Code (Optional (FromNode dom node)))
    ) => Parse (ParseNode node) where
  parse = fmap ParseNode <$> go (Proxy @(FromNode dom node))
    where
      go :: forall a. a ~ FromNode dom node
         => Proxy (a :: dom) -> Parser (Maybe (Node a))
      go p =
          P.tag'
            nodeName
            (parseAttrs p)
            (\attrs -> P.withContext (nodeName :) $ do
               children <- P.many' parseChild
               mkNode attrs children
            )
        where
          nodeName :: String
          nodeName = case show (typeRep p) of
                       '\'' : n -> n
                       n        -> n

          parseChild :: Parser (Maybe (Child a))
          parseChild = P.choose [
                fmap ChildRequired <$> parseRequired
              , fmap ChildOptional <$> parseOptional
              ]

          mkNode :: Attrs a -> [Child a] -> Parser (Node a)
          mkNode attrs children = do
              ctxt     <- P.getContext
              required <- complete ctxt required'
              return $ Node {..}
            where
              required' :: [Partial (Required a)]
              optional  :: [Optional a]
              (required', optional) = partitionChildren children

{-------------------------------------------------------------------------------
  Combine optional and required children (impose ordering later)
-------------------------------------------------------------------------------}

data Child (a :: dom) where
    ChildRequired :: Partial (Required a) -> Child a
    ChildOptional :: Optional a -> Child a

childToEither :: Child a -> Either (Partial (Required a)) (Optional a)
childToEither (ChildRequired c) = Left c
childToEither (ChildOptional c) = Right c

partitionChildren :: [Child a] -> ([Partial (Required a)], [Optional a])
partitionChildren = partitionEithers . map childToEither

{-------------------------------------------------------------------------------
  Parse required children
-------------------------------------------------------------------------------}

parseRequired ::
     forall a xs. (
          Generic a
        , SingletonSatisfying (All Parse) (Code a)
        , Single (Code a) ~ xs
        )
  => Parser (Maybe (Partial a))
parseRequired = P.choose $ hcollapse parsers
  where
    parsers :: NP (K (Parser (Maybe (Partial a)))) xs
    parsers =
        hcmap
          (Proxy @Parse)
          (\i -> K $ fmap (emb i) <$> parse)
          indices

    emb :: Index xs b -> b -> Partial a
    emb i b = Partial $ hexpand [] (inject i [b])

{-------------------------------------------------------------------------------
  Parse optional children
-------------------------------------------------------------------------------}

parseOptional ::
     forall a. (Generic a, All (SingletonSatisfying Parse) (Code a))
  => Parser (Maybe a)
parseOptional = P.choose $ hcollapse parsers
  where
    parsers :: NP (K (Parser (Maybe a))) (Code a)
    parsers =
        hcmap
          (Proxy @(SingletonSatisfying Parse))
          (\i -> K $ fmap (emb i) <$> parse)
          indices

    emb :: Index (Code a) '[b] -> b -> a
    emb i = to . SOP . inject i . (:* Nil) . I

{-------------------------------------------------------------------------------
  Parse attributes
-------------------------------------------------------------------------------}

parseAttrs ::
     forall dom (a :: dom) xs.
     ( HasDatatypeInfo (Attrs a)
     , SingletonSatisfying (All ParseAttr) (Code (Attrs a))
     , Single (Code (Attrs a)) ~ xs
     )
  => Proxy a
  -> AttrParser (Attrs a)
parseAttrs _ =
    to . SOP . Z <$> (hsequence parsers <* ignoreAttrs)
  where
    -- A parser for every attribute
    parsers :: NP AttrParser xs
    parsers = hcmap (Proxy @ParseAttr) parseField (fieldInfo (Proxy @(Attrs a)))

    parseField :: ParseAttr attr => FieldInfo attr -> AttrParser attr
    parseField (FieldInfo n) = parseAttr (fromString n)
