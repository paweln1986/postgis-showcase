module Scope.Data where

import Data.Aeson (FromJSON, ToJSON)
import Data.Geospatial (GeoPolygon)
import Data.OpenApi (ToSchema)
import Data.OpenApi.Internal.Schema (declareNamedSchema)

data Point = Point
    { x :: Int
    , y :: Int
    }
    deriving stock (Show)

data Line = Line
    { start :: Point
    , end :: Point
    }
    deriving stock (Show)

newtype Polygon = Polygon {unGeoPoly :: GeoPolygon }
    deriving newtype (Show, ToJSON, FromJSON)

instance ToSchema Polygon where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy GeoPolygon)

newtype ScopeId = ScopeId {unScopeId :: Int}
    deriving newtype (Show, FromJSON, ToJSON, ToSchema)

newtype ScopeDescription = ScopeDescription {unScopeDescription :: Text}
    deriving newtype (Show, FromJSON, ToJSON, ToSchema)

newtype ScopeName = ScopeName {unScopeName :: Text}
    deriving newtype (Show, FromJSON, ToJSON, ToSchema)
