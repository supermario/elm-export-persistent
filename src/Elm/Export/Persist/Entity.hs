-- |
-- Module      :  Elm.Export.Persist.Ent
-- Copyright   :  (C) 2016-17 William Casarin
-- License     :  MIT
-- Maintainer  :  William Casarin <bill@casarin.me>
-- Stability   :  experimental
-- Portability :  non-portable

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Elm.Export.Persist.Entity where

import Database.Persist
import Elm
import Data.Proxy
import Data.Text
import Data.Monoid ((<>))

-- | `ElmType` instances for Persistent `Entity a` types
--
-- >>> toElmTypeSource (Proxy :: Proxy (Entity User))
-- "type alias User = { userId : Maybe Int, userName : String }"

elmIdField :: Text -> ElmValue
elmIdField keyfield =
  ElmField keyfield (ElmPrimitiveRef (EMaybe (ElmPrimitive EInt)))

instance (ElmType a) => ElmType (Entity a) where
  toElmType _ =
    case toElmType (Proxy :: Proxy a) of
      ElmDatatype name (RecordConstructor x ev) ->
        ElmDatatype name (RecordConstructor x
                            (Values (elmIdField (toLower name <> "Id")) ev))
      x -> x
