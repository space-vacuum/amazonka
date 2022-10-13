{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Module      : Gen.JSON
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)

module Gen.JSON where

import Control.Error
import Control.Monad.Except

import Data.Aeson       hiding (decode)
import Data.Aeson.Shim
import Data.Aeson.Types
import Data.Bifunctor
import Data.ByteString  (ByteString)
import Data.List

import Gen.Formatting
import Gen.IO
import Gen.Types

import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict  as Map
import qualified Data.Text.Lazy       as LText
import qualified Text.EDE             as EDE

required :: MonadIO m => Path -> ExceptT Error m Object
required = readBSFile >=> hoistEither . decode

optional :: MonadIO m => Path -> ExceptT Error m Object
optional f = readBSFile f `catchError` const (return "{}")
    >>= hoistEither . decode

objectErr :: ToJSON a => String -> a -> Either Error Object
objectErr n =
      note (format ("Failed to extract JSON object from value " % string) n)
    . fmap fwd
    . EDE.fromValue
    . toJSON

decode :: ByteString -> Either Error Object
decode = first LText.pack . eitherDecode' . LBS.fromStrict

parse :: FromJSON a => Object -> Either Error a
parse = first LText.pack . parseEither parseJSON . Object

merge :: [Object] -> Object
merge xs = fwd $ foldl' go mempty ys
  where
    ys = bwd <$> xs

    go = Map.unionWith value

    value :: Value -> Value -> Value
    value l r =
        case (l, r) of
            (Object x, Object y) -> Object $ fwd (bwd x `go` bwd y)
            (_,        _)        -> l
