CREATE TABLE scope (
  scope_id  SERIAL PRIMARY KEY,
  scope_name TEXT NOT NULL,
  scope_description TEXT,
  scope_polygon GEOMETRY(POLYGON, 4326) NOT NULL
);

create index scope_polygon_idx on scope using GIST(scope_polygon);
