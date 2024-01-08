{-# LANGUAGE QuasiQuotes #-}

module Scope.Queries (selectScope, selectAllScope, insertScope) where

import Data.Aeson (encode)
import Data.Geospatial (GeoPolygon)
import Data.Profunctor (Profunctor (dimap, lmap))
import Data.Vector (Vector)
import Hasql.Session (statement)
import Hasql.Session qualified as Session
import Hasql.Statement (Statement (..), refineResult)
import Hasql.TH (singletonStatement, vectorStatement)
import Scope.Http.Payloads (Scope (..))
import Utils.Aeson (encodeJson)
import Utils.Data (ScopeDescription (ScopeDescription), ScopeId (ScopeId), ScopeName (ScopeName))

insertScope :: (ScopeName, Maybe ScopeDescription, GeoPolygon) -> Session.Session ScopeId
insertScope params = statement params (dimap toParameters coerce sql)
  where
    sql =
      [singletonStatement|
        insert into scope (scope_name, scope_description, scope_polygon) 
        values ($1 :: text, $2 :: text?, ST_GeomFromGeoJSON($3 :: text)) 
        returning scope_id :: int8
      |]
    toParameters (scopeName, scopeDescription, geoPolygon) =
      (coerce scopeName, coerce scopeDescription, decodeUtf8 $ encode geoPolygon)

selectScope :: Statement ScopeId (Vector GeoPolygon)
selectScope = lmap coerce $ refineResult (mapM encodeJson) sql
  where
    sql =
      [vectorStatement|
        select ST_AsGeoJSON(scope_polygon)::text from scope where scope_id = $1 :: int8
      |]

selectAllScope :: Session.Session (Vector Scope)
selectAllScope = statement () $ refineResult (mapM toScope) sql
  where
    sql =
      [vectorStatement|
        select scope_id::int8, scope_name::text, scope_description::text?, ST_AsGeoJSON(scope_polygon)::text from scope
      |]
    toScope (scopeId, name, description, geojson) =
      Scope (coerce scopeId) (coerce name) (coerce description) <$> encodeJson geojson
