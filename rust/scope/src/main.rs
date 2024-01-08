mod http;
mod scopes;
mod utils;
mod antennas;

use anyhow::Context;
use anyhow::Result;
use http::server;
use sqlx::postgres::PgPoolOptions;
use std::sync::Arc;

#[tokio::main]
async fn main() -> Result<()> {
    env_logger::init();

    let db = PgPoolOptions::new()
        .max_connections(5)
        .connect("postgresql://postgres:postgres@localhost/test")
        .await
        .context("could not connect to database_url")?;

    println!("Hello world");
    server::serve(Arc::new(db)).await
}
