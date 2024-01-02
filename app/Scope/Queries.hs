module Scope.Queries (selectScope, selectAllScope, insertScope) where

import Contravariant.Extras (contrazip3)
import Data.Aeson (FromJSON, ToJSON, encode)
import Data.Aeson.Decoding qualified as Aeson
import Data.Geospatial (GeoPolygon)
import Data.Vector (Vector)
import Hasql.Decoders (refine)
import Hasql.Decoders qualified as Decoders
import Hasql.Decoders qualified as HD
import Hasql.Encoders (param)
import Hasql.Encoders qualified as Encoders
import Hasql.Session (statement)
import Hasql.Session qualified as Session
import Hasql.Statement (Statement (..))
import Scope.Data (ScopeDescription (ScopeDescription, unScopeDescription), ScopeId (..), ScopeName (ScopeName, unScopeName))
import Scope.Http.Payloads (Scope (..))

insertScope :: (ScopeName, Maybe ScopeDescription, GeoPolygon) -> Session.Session ScopeId
insertScope params = statement params $ Statement sql encoder decoder True
 where
  sql = "insert into scope(scope_name, scoee_description, scope_polygon) values ($1, $2, ST_GeomFromGeoJSON($3)) returning scope_id"
  decoder = Decoders.singleRow $ Decoders.column $ Decoders.nonNullable scopeIdDecoder
  encoder = contrazip3 scopeNameEncoder scopeDescriptionEncoder geoPolygonEncoder

selectScope :: Statement ScopeId (Vector GeoPolygon)
selectScope = Statement sql scopeIdEncoder decoder True
 where
  sql = "select ST_AsGeoJSON(scope_polygon) from scope where scope_id = $1"
  decoder = Decoders.rowVector row
  row = Decoders.column (Decoders.nonNullable geometry)

selectAllScope :: Session.Session (Vector Scope)
selectAllScope = statement () $ Statement sql Encoders.noParams decoder True
 where
  sql = "select scope_id, scope_name, scoee_description, ST_AsGeoJSON(scope_polygon) from scope"
  decoder = Decoders.rowVector row
  row =
    Scope
      <$> Decoders.column (Decoders.nonNullable scopeIdDecoder)
      <*> Decoders.column (Decoders.nonNullable scopeNameDecoder)
      <*> Decoders.column (Decoders.nullable scopeDescriptionDecoder)
      <*> Decoders.column (Decoders.nonNullable geometry)

-- saveScope :: Statement (ScopeName, )

scopeIdEncoder :: Encoders.Params ScopeId
scopeIdEncoder = fromIntegral . unScopeId >$< param (Encoders.nonNullable Encoders.int8)

scopeNameEncoder :: Encoders.Params ScopeName
scopeNameEncoder = unScopeName >$< param (Encoders.nonNullable Encoders.text)

scopeDescriptionEncoder :: Encoders.Params (Maybe ScopeDescription)
scopeDescriptionEncoder = param (Encoders.nullable (unScopeDescription >$< Encoders.text))

geoPolygonEncoder :: Encoders.Params GeoPolygon
geoPolygonEncoder = param (Encoders.nonNullable geometryEV)

scopeIdDecoder :: HD.Value ScopeId
scopeIdDecoder = ScopeId . fromIntegral <$> HD.int8

scopeNameDecoder :: HD.Value ScopeName
scopeNameDecoder = ScopeName <$> HD.text

scopeDescriptionDecoder :: HD.Value ScopeDescription
scopeDescriptionDecoder = ScopeDescription <$> HD.text

geometry :: (FromJSON a) => HD.Value a
geometry = refine (first toText) $ Aeson.eitherDecodeStrict . encodeUtf8 <$> HD.text

geometryEV :: (ToJSON a) => Encoders.Value a
geometryEV = (decodeUtf8 . encode) >$< Encoders.text
