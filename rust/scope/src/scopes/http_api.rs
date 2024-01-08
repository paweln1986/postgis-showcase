use axum::{extract::State, routing::{get, post}, Json, Router, debug_handler};

use crate::{http::api_context::ApiContext, utils::AppError};

use anyhow::Result;

use super::scope_repository::{Scope, CreateScopeRequest};

pub fn router() -> Router<ApiContext> {
    Router::new()
        .route("/api/scopes", get(get_scopes))
        .route("/api/create-scope", post(save_scopes))
}

async fn get_scopes(State(state): State<ApiContext>) -> Result<Json<Vec<Scope>>, AppError> {
    let res = state.scope_store.all_scopes().await?;
    Ok(Json(res))
}

#[debug_handler]
async fn save_scopes(State(state): State<ApiContext>, Json(req): Json<CreateScopeRequest>) -> Result<Json<i32>, AppError> {
    let res = state.scope_store.save_scope(req).await?;
    Ok(Json(res.scope_id))
}
