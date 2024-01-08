use std::sync::Arc;

use anyhow::Result;
use async_trait::async_trait;
use geojson::{GeoJson, JsonValue};
use sqlx::PgPool;

pub type DynScopeRepository = Arc<dyn ScopeRepository + Send + Sync>;

#[derive(serde::Serialize)]
pub struct Scope {
    scope_id: i32,
    scope_name: String,
    scope_description: Option<String>,
    polygon: Option<JsonValue>,
}

#[derive(serde::Serialize, serde::Deserialize)]
pub struct CreateScopeRequest {
    pub name: String,
    pub description: Option<String>,
    pub polygon: sqlx::types::Json<GeoJson>,
}

pub struct PsqlScopeRepository {
    pool: Arc<PgPool>,
}

pub struct ScopeId {
    pub scope_id: i32,
}

impl PsqlScopeRepository {
    pub fn new(pool: Arc<PgPool>) -> Self {
        Self { pool }
    }
}

#[async_trait]
pub trait ScopeRepository {
    async fn all_scopes(&self) -> Result<Vec<Scope>>;
    async fn save_scope(&self, scope_request: CreateScopeRequest) -> Result<ScopeId>;
}

#[async_trait]
impl ScopeRepository for PsqlScopeRepository {
    async fn all_scopes(&self) -> Result<Vec<Scope>> {
        let pool: Arc<sqlx::Pool<sqlx::Postgres>> = Arc::clone(&self.pool);
        sqlx::query_as!(
            Scope,
            r#"
                select 
                    scope_id, 
                    scope_name, 
                    scope_description, 
                    ST_AsGeoJSON(scope_polygon) :: json as polygon
                from scope
            "#
        )
        .fetch_all(&*pool)
        .await
        .map_err(anyhow::Error::from)
    }

    async fn save_scope(&self, scope_request: CreateScopeRequest) -> Result<ScopeId> {
        let pool = Arc::clone(&self.pool);
        let geojson = scope_request.polygon.encode_to_string();
        sqlx::query_as!(
            ScopeId,
            r#"
                insert into scope (scope_name, scope_description, scope_polygon) 
                values ($1, $2, ST_GeomFromGeoJSON($3)) 
                returning scope_id
            "#,
            scope_request.name,
            scope_request.description,
            geojson
        )
        .fetch_one(&*pool)
        .await
        .map_err(anyhow::Error::from)
    }
}
