CREATE TABLE scope (
  scope_id  SERIAL PRIMARY KEY,
  scope_name TEXT NOT NULL,
  scoee_name TEXT,
  scope_polygon GEOMETRY(POLYGON, 4326) NOT NULL
);