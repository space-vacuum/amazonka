{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RoleAnnotations            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module      : Network.AWS.Data.Map
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Data.Map
    ( Map (..)
    , _Map
    , parseXMLMap
    , parseHeadersMap
    , toQueryMap
    ) where

import           Control.DeepSeq
import           Data.Aeson
import           Data.Aeson.Shim
import           Data.Bifunctor
import qualified Data.ByteString             as BS
import qualified Data.CaseInsensitive        as CI
import           Data.Coerce
import           Data.Data                   (Data, Typeable)
import           Data.Hashable
import           Data.HashMap.Strict         (HashMap)
import           Data.Maybe
import qualified Data.Text.Encoding          as Text
import           GHC.Exts
import           GHC.Generics                (Generic)
import           Network.AWS.Data.ByteString
import           Network.AWS.Data.Headers
import           Network.AWS.Data.Query
import           Network.AWS.Data.Text
import           Network.AWS.Data.XML
import           Network.AWS.Lens            (Iso', iso)
import           Network.HTTP.Types          (ResponseHeaders)
import           Text.XML                    (Node)

#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.KeyMap as KM
#else
import qualified Data.HashMap.Strict as Map
import qualified Data.HashMap.Strict as KM
type Key = Text
#endif

newtype Map k v =
#if MIN_VERSION_aeson(2,0,0)
  Map { toMap :: KM.KeyMap v }
#else
  Map { toMap :: HashMap k v }
#endif
    deriving
        ( Functor
        , Foldable
        , Traversable
        , Monoid
        , Semigroup
        , Eq
        , Read
        , Show
        , Data
        , Typeable
        , Generic
        )

type role Map nominal representational

#if MIN_VERSION_aeson(2,0,0)
_Map :: (Coercible a b, Coercible b a) => Iso' (Map Text a) (HashMap Text b)
_Map = iso (bwd . coerce . toMap) (Map . fwd . coerce)

instance (Hashable k, Hashable v) => Hashable (Map k v)
instance (NFData k,   NFData   v) => NFData   (Map k v)

instance (Hashable k, Eq k) => IsList (Map k v) where
   type Item (Map k v) = (Text, v)

   fromList = Map . fromMapList
   toList = toMapList . toMap

fromMapList :: [(Text, v)] -> KM.KeyMap v
fromMapList = KM.fromList . fmap (first fwdKey)

toMapList :: KM.KeyMap c -> [(Text, c)]
toMapList = fmap (first bwdKey) . KM.toList

instance (Eq k, Hashable k, FromText k, FromJSON v) => FromJSON (Map k v) where
    parseJSON = withObject "HashMap" (fmap fromList . traverse f . toMapList)
      where
        f (k, v) = (,)
            <$> either fail return (fromText k)
            <*> parseJSON v

instance (Eq k, Hashable k, ToText k, ToJSON v) => ToJSON (Map k v) where
    toJSON = Object . fromMapList . map (bimap toText toJSON) . toList
#else
_Map :: (Coercible a b, Coercible b a) => Iso' (Map k a) (HashMap k b)
_Map = iso (coerce . toMap) (Map . coerce)

instance (Hashable k, Hashable v) => Hashable (Map k v)
instance (NFData k,   NFData   v) => NFData   (Map k v)

instance (Hashable k, Eq k) => IsList (Map k v) where
   type Item (Map k v) = (k, v)

   fromList = Map . Map.fromList
   toList   = Map.toList . toMap

instance (Eq k, Hashable k, FromText k, FromJSON v) => FromJSON (Map k v) where
    parseJSON o = withObject "HashMap" (fmap fromList . traverse f $ toList o)
      where
        f (k, v) = (,)
            <$> either fail return (fromText k)
            <*> parseJSON v

instance (Eq k, Hashable k, ToText k, ToJSON v) => ToJSON (Map k v) where
    toJSON = Object . fromList . map (bimap toText toJSON) . toList
#endif

instance (Eq k, Hashable k, ToByteString k, ToText v) => ToHeader (Map k v) where
    toHeader p = map (bimap k v) . toList
      where
        k = mappend p . CI.mk . toBS
        v = Text.encodeUtf8 . toText

parseXMLMap :: (Eq k, Hashable k, FromText k, FromXML v)
            => Text
            -> Text
            -> Text
            -> [Node]
            -> Either String (Map k v)
parseXMLMap e k v = fmap fromList . traverse f . mapMaybe (childNodesOf e)
  where
    f ns = (,)
       <$> (ns .@ k >>= fromText)
       <*>  ns .@ v

parseHeadersMap :: FromText a
                => ByteString
                -> ResponseHeaders
                -> Either String (Map Text a)
parseHeadersMap p = fmap fromList . traverse g . filter f
  where
    f = BS.isPrefixOf p . CI.foldedCase . fst

    g (k, v) = (Text.decodeUtf8 . BS.drop n $ CI.original k,) <$>
        fromText (Text.decodeUtf8 v)

    n = BS.length p

toQueryMap :: (Hashable k, Eq k, ToQuery k, ToQuery v)
           => ByteString
           -> ByteString
           -> ByteString
           -> Map k v
           -> QueryString
toQueryMap e k v = toQueryList e . map f . toList
  where
    f (x, y) = QList [k =: toQuery x, v =: toQuery y]
