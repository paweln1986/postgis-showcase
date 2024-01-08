CREATE TABLE antenna (
  antenna_id  SERIAL PRIMARY KEY,
  antenna_direction SMALLINT NOT NULL,
  antenna_location GEOMETRY(POINT, 4326) NOT NULL,
  antenna_type TEXT NOT NULL
);

create index antenna_location_idx on antenna using gist(antenna_location);

-- select 
--   jsonb_build_object(
--           'type', 'Feature', 
--           'id', row_number() over (),
--           'geometry', cast(ST_AsGeoJSON(st_centroid) as jsonb),
--           'properties', jsonb_object('{result_type, Cluster}') || jsonb_build_object('size', st_numgeometries)
--         )
-- from (select ST_Centroid(ST_MinimumBoundingCircle(unnest(cluster))), ST_NumGeometries(unnest(cluster)) from (select ST_ClusterWithin(antenna_location, 0.001) as cluster from antenna where ST_Within(antenna_location, ST_MakeEnvelope(16.200027465820316,50.9151558824997,17.295913696289066,51.29455864325789, 4326))));

-- select jsonb_build_object(                                                                                                                                                                                                                                              
--           'type', 'Feature', 
--           'id', row_number() over (),
--           'geometry', cast(ST_AsGeoJSON(ST_Centroid(center)) as jsonb),
--           'properties', jsonb_object('{result_type, Cluster}') || jsonb_build_object('size', st_numgeometries, 'radius', radius)
--         )
--         from (select cluster.kmean, (ST_MinimumBoundingRadius(ST_Collect(antenna_location))).*, count(antenna_location) as st_numgeometries from (SELECT antenna_id, ST_ClusterKMeans(antenna_location, 20) OVER() AS kmean, antenna_location from antenna) as cluster group by cluster.kmean)


-- select 
--         jsonb_build_object(
--                 'type', 'Feature', 
--                 'id', row_number() over (),
--                 'geometry', cast(ST_AsGeoJSON(ST_Centroid(center)) as jsonb),
--                 'properties', jsonb_object('{result_type, Cluster}') || jsonb_build_object('size', st_numgeometries, 'radius', radius)
--               ) ::text
--         from (select cluster.kmean, (ST_MinimumBoundingRadius(ST_Collect(antenna_location))).*, count(antenna_location) as st_numgeometries from (SELECT antenna_id, ST_ClusterKMeans(antenna_location, 20) OVER() AS kmean, antenna_location from antenna where ST_Within(antenna_location, ST_MakeEnvelope($1 :: float8, $2 :: float8, $3 :: float8, $4 :: float8, 4326))) as cluster group by cluster.kmean)



-- select 
--         jsonb_build_object(
--                 'type', 'Feature', 
--                 'id', row_number() over (),
--                 'geometry', cast(ST_AsGeoJSON(ST_Centroid(center)) as jsonb),
--                 'properties', jsonb_object('{tag, ClusterAntenna}') || jsonb_build_object('size', st_numgeometries, 'radius', radius)
--               ) :: text 
--         from (
--           select cluster.kmean, (ST_MinimumBoundingRadius(ST_Collect(antenna_location))).*, count(antenna_location) as st_numgeometries 
--           from 
--           (
--             SELECT antenna_id, ST_ClusterKMeans(antenna_location, 20) OVER() AS kmean, antenna_location 
--             from antenna 
--             where ST_Within(antenna_location, ST_MakeEnvelope($1 :: float8, $2 :: float8, $3 :: float8, $4 :: float8, 4326))
--           ) as cluster group by cluster.kmean
--         )

-- select 
--         jsonb_build_object(
--                 'type', 'Feature', 
--                 'id', row_number() over (),
--                 'geometry', cast(ST_AsGeoJSON(ST_Centroid(center)) as jsonb),
--                 'properties', jsonb_object('{tag, ClusterAntenna}') || jsonb_build_object('size', st_numgeometries, 'radius', radius)
--               )
--         from 
--           (
--           select cluster.kmean, (ST_MinimumBoundingRadius(ST_Collect(antenna_location))).*, count(antenna_location) as st_numgeometries 
--           from 
--           (
--             SELECT ST_ClusterKMeans(antenna_location, 20) over (), antenna_id AS kmean, antenna_location 
--             from 
--             antenna where ST_Within(antenna_location, ST_MakeEnvelope(17.153434753417972,50.4218626237537,17.42740631103516,50.518010982572044, 4326))
--           ) as cluster group by cluster.kmean
--         )
      