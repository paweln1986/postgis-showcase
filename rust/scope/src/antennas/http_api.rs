use axum::{
    extract::{Path, Query, State},
    routing::get,
    Json, Router,
};
use geojson::JsonValue;
use serde::Deserialize;

use crate::{http::api_context::ApiContext, utils::AppError};

use anyhow::Result;

#[derive(Deserialize, Debug)]
struct AntennaParams {
    bbox: String,
    zoom: i32,
}

#[derive(Deserialize, Debug)]
struct AntennaBands {
    band: Option<String>,
}

pub fn router() -> Router<ApiContext> {
    Router::new()
        .route("/api/scopes/:scopeId/antenna", get(get_scope_antennas))
        .route("/api/antenna", get(get_antennas_inside_bbox))
}

async fn get_scope_antennas(
    State(state): State<ApiContext>,
    Path(scope_id): Path<i32>,
    Query(params): Query<AntennaBands>,
) -> Result<Json<Vec<JsonValue>>, AppError> {
    let all_bands = vec!["GSM", "WCDMA", "LTE", "FiveG"]
        .into_iter()
        .map(|a| String::from(a))
        .collect();
    let bands = params.band.map(|a| vec![a]).unwrap_or(all_bands);
    let res = state
        .antenna_store
        .select_antennas_inside_scope(scope_id, bands)
        .await?;
    Ok(Json(res))
}

async fn get_antennas_inside_bbox(
    State(state): State<ApiContext>,
    Query(params): Query<AntennaParams>,
) -> Result<Json<Vec<JsonValue>>, AppError> {
    let bbox: Vec<f64> = params
        .bbox
        .split(',')
        .map(|a| a.parse::<f64>().unwrap())
        .collect();
    if params.zoom < 10 {
        let res = state
            .antenna_store
            .select_clustered_antennas_inside_bbox(
                (bbox[0], bbox[1], bbox[2], bbox[3]),
                params.zoom,
            )
            .await?;
        Ok(Json(res))
    } else {
        let res = state
            .antenna_store
            .select_antennas_inside_bbox((bbox[0], bbox[1], bbox[2], bbox[3]))
            .await?;
        Ok(Json(res))
        
    }
}
