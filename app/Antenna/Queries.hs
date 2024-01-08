{-# LANGUAGE QuasiQuotes #-}

module Antenna.Queries where

import Antenna.Http.Payloads
  ( AntennaProperties,
    AntennaType,
    Direction (Direction),
  )
import Data.Aeson (encode)
import Data.Geospatial (GeoFeature, GeoPoint)
import Data.Profunctor (Profunctor (dimap))
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Hasql.Session (Session, statement)
import Hasql.Statement (refineResult)
import Hasql.TH (resultlessStatement, vectorStatement)
import Utils.Aeson (encodeJson)
import Utils.Data (Coords, ScopeId (..))

insertAntenna :: Vector (Direction, GeoPoint, AntennaType) -> Session ()
insertAntenna params = statement params (dimap toArrayParameters coerce sql)
  where
    sql =
      [resultlessStatement|
        insert into antenna (antenna_type, antenna_direction, antenna_location) 
        select antenna_type, antenna_direction, ST_AsGeoJSON(antenna_location) 
        from unnest ($1 :: text[], $2 :: int2[], $3 :: text[]) AS t(antenna_type, antenna_direction, antenna_location)
      |]
    toArrayParameters = Vector.unzip3 . fmap toParameters
    toParameters (direction, point, antennaType) =
      (show antennaType, coerce direction, decodeUtf8 $ encode point)

selectAntennasInsideScope :: ScopeId -> Vector AntennaType -> Session (Vector (GeoFeature AntennaProperties))
selectAntennasInsideScope scopeId filters = statement (scopeId.value, fmap show filters) $ refineResult (mapM encodeJson) sql
  where
    sql =
      [vectorStatement|
        select jsonb_build_object(
          'type', 'Feature', 
          'id', antenna_id,
          'geometry', cast(ST_AsGeoJSON(antenna_location) as jsonb),
          'properties', jsonb_object('{tag, SingleAntenna}') || to_jsonb(inputs) - 'antenna_id' - 'antenna_location'  
        )::text from 
        (select antenna_id, antenna_direction, antenna_location, antenna_type from antenna,scope where ST_Within(antenna_location, scope_polygon) and scope_id = $1::int8 and antenna_type = ANY($2 :: text[])) as inputs
      |]

selectAntennasInsideBBox :: Coords -> Session (Vector (GeoFeature AntennaProperties))
selectAntennasInsideBBox ((x1, y1), (x2, y2)) = statement (x1, y1, x2, y2) $ refineResult (mapM encodeJson) sql
  where
    sql =
      [vectorStatement|
        select jsonb_build_object(
          'type', 'Feature', 
          'id', antenna_id,
          'geometry', cast(ST_AsGeoJSON(antenna_location) as jsonb),
          'properties', jsonb_object('{tag, SingleAntenna}') || to_jsonb(inputs) - 'antenna_id' - 'antenna_location'  
        )::text from 
        (select antenna_id, antenna_direction, antenna_location, antenna_type from antenna,scope 
        where ST_Within(antenna_location, ST_MakeEnvelope($1 :: float8, $2 :: float8, $3 :: float8, $4 :: float8, 4326))) as inputs
      |]

selectClusteredAntennasBBox :: Coords -> Int16 -> Session (Vector (GeoFeature AntennaProperties))
selectClusteredAntennasBBox ((x1, y1), (x2, y2)) zoom = statement (x1, y1, x2, y2, zoom) $ refineResult (mapM encodeJson) sql
  where
    sql =
      [vectorStatement|
      select 
        jsonb_build_object(
                'type', 'Feature', 
                'id', row_number() over (),
                'geometry', cast(ST_AsGeoJSON(ST_GeometricMedian(ST_Transform(center, 4326))) as jsonb),
                'properties', jsonb_object('{tag, ClusterAntenna}') || jsonb_build_object('size', st_numgeometries, 'radius', radius)
              ) :: text 
        from (
          select cluster.kmean, (ST_MinimumBoundingRadius(ST_Collect(ST_Transform(antenna_location, 3857)))).*, count(antenna_location) as st_numgeometries 
          from 
          (
            SELECT antenna_id, ST_ClusterKMeans(antenna_location, $5 :: int2) OVER () AS kmean, antenna_location 
            from antenna 
            where ST_Within(antenna_location, ST_MakeEnvelope($1 :: float8, $2 :: float8, $3 :: float8, $4 :: float8, 4326))
          ) as cluster group by cluster.kmean
        )
      |]
