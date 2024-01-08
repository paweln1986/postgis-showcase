use std::sync::Arc;

use anyhow::Result;
use async_trait::async_trait;
use geojson::JsonValue;
use sqlx::PgPool;

pub type DynAntennaRepository = Arc<dyn AntennaRepository + Send + Sync>;

pub struct PsqlAntennaRepository {
    pool: Arc<PgPool>,
}

impl PsqlAntennaRepository {
    pub fn new(pool: Arc<PgPool>) -> Self {
        Self { pool }
    }
}

struct AntennaGeoJson {
    value: JsonValue,
}

#[async_trait]
pub trait AntennaRepository {
    async fn select_antennas_inside_scope(
        &self,
        scope_id: i32,
        bands: Vec<String>,
    ) -> Result<Vec<JsonValue>>;

    async fn select_clustered_antennas_inside_bbox(
        &self,
        bbox: (f64, f64, f64, f64),
        zoom: i32,
    ) -> Result<Vec<JsonValue>>;

    async fn select_antennas_inside_bbox(
        &self,
        bbox: (f64, f64, f64, f64),
    ) -> Result<Vec<JsonValue>>;
}

#[async_trait]
impl AntennaRepository for PsqlAntennaRepository {
    async fn select_antennas_inside_scope(
        &self,
        scope_id: i32,
        bands: Vec<String>,
    ) -> Result<Vec<JsonValue>> {
        let pool = Arc::clone(&self.pool);
        sqlx::query_as!(
            AntennaGeoJson,
            r#"
            select jsonb_build_object(
                'type', 'Feature', 
                'id', antenna_id,
                'geometry', cast(ST_AsGeoJSON(antenna_location) as jsonb),
                'properties', jsonb_object('{tag, SingleAntenna}') || to_jsonb(inputs) - 'antenna_id' - 'antenna_location'  
              ) as value from 
              (select antenna_id, antenna_direction, antenna_location, antenna_type from antenna,scope where ST_Within(antenna_location, scope_polygon) and scope_id = $1 and antenna_type = ANY($2)) as inputs
            "#
        , scope_id, &bands)
        .fetch_all(&*pool)
        .await
        .map_err(anyhow::Error::from)
        .map(|antennas| antennas.into_iter().map(|res| res.value).collect())
    }

    async fn select_antennas_inside_bbox(
        &self,
        bbox: (f64, f64, f64, f64),
    ) -> Result<Vec<JsonValue>> {
        let pool = Arc::clone(&self.pool);
        sqlx::query_as!(
            AntennaGeoJson,
            r#"
            select jsonb_build_object(
                'type', 'Feature', 
                'id', antenna_id,
                'geometry', cast(ST_AsGeoJSON(antenna_location) as jsonb),
                'properties', jsonb_object('{tag, SingleAntenna}') || to_jsonb(inputs) - 'antenna_id' - 'antenna_location'  
              ) as value from 
              (select antenna_id, antenna_direction, antenna_location, antenna_type from antenna,scope 
              where ST_Within(antenna_location, ST_MakeEnvelope($1, $2, $3, $4, 4326))) as inputs
            "#,
            bbox.0,
            bbox.1,
            bbox.2,
            bbox.3
        ).fetch_all(&*pool)
        .await
        .map_err(anyhow::Error::from)
        .map(|antennas| antennas.into_iter().map(|res| res.value).collect())
    }

    async fn select_clustered_antennas_inside_bbox(
        &self,
        bbox: (f64, f64, f64, f64),
        zoom: i32,
    ) -> Result<Vec<JsonValue>> {
        let pool = Arc::clone(&self.pool);
        sqlx::query_as!(
                AntennaGeoJson,
                r#"
                    select 
                        jsonb_build_object(
                                'type', 'Feature', 
                                'id', row_number() over (),
                                'geometry', cast(ST_AsGeoJSON(ST_GeometricMedian(ST_Transform(center, 4326))) as jsonb),
                                'properties', jsonb_object('{tag, ClusterAntenna}') || jsonb_build_object('size', st_numgeometries, 'radius', radius)
                              ) as value 
                    from (
                          select cluster.kmean, (ST_MinimumBoundingRadius(ST_Collect(ST_Transform(antenna_location, 3857)))).*, count(antenna_location) as st_numgeometries 
                          from 
                          (
                            SELECT antenna_id, ST_ClusterKMeans(antenna_location, $5) OVER () AS kmean, antenna_location 
                            from antenna 
                            where ST_Within(antenna_location, ST_MakeEnvelope($1, $2, $3, $4, 4326))
                          ) as cluster group by cluster.kmean
                        )
                    "#,
                bbox.0,
                bbox.1,
                bbox.2,
                bbox.3,
                zoom
            ).fetch_all(&*pool)
            .await
            .map_err(anyhow::Error::from)
            .map(|antennas| antennas.into_iter().map(|res| res.value).collect())
    }
}
