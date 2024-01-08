use std::sync::Arc;

use axum::Router;
use sqlx::PgPool;

use crate::{scopes::{
    self,
    scope_repository::{DynScopeRepository, PsqlScopeRepository},
}, antennas::{antenna_repository::{PsqlAntennaRepository, DynAntennaRepository}, self}};

use super::api_context::ApiContext;

pub async fn serve(db: Arc<PgPool>) -> anyhow::Result<()> {
    let api_context = ApiContext {
        scope_store: Arc::new(PsqlScopeRepository::new(Arc::clone(&db))) as DynScopeRepository,
        antenna_store: Arc::new(PsqlAntennaRepository::new(Arc::clone(&db))) as DynAntennaRepository
    };

    let app = api_router(api_context);
    let listener = tokio::net::TcpListener::bind("0.0.0.0:8000").await.unwrap();
    axum::serve(listener, app)
        .await
        .map_err(anyhow::Error::from)
}

fn api_router(api_context: ApiContext) -> Router {
    Router::new()
        .merge(scopes::http_api::router())
        .merge(antennas::http_api::router())
        .with_state(api_context)
}
