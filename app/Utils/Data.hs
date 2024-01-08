module Utils.Data where

import Data.Aeson (FromJSON, ToJSON)
import Data.Geospatial (GeoPolygon)
import Data.OpenApi (ToParamSchema, ToSchema)
import Data.OpenApi.Internal.Schema (declareNamedSchema)
import Data.Text (splitOn)
import Data.Text.Read (double)
import Servant (FromHttpApiData)

newtype Polygon = Polygon {value :: GeoPolygon}
  deriving newtype (Show, ToJSON, FromJSON)

instance ToSchema Polygon where
  declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy String)

newtype ScopeId = ScopeId {value :: Int64}
  deriving newtype (Show, FromJSON, ToJSON, ToSchema, FromHttpApiData, ToParamSchema)

newtype BBox = BBox {value :: Text}
  deriving newtype (Show, FromJSON, ToJSON, ToSchema, FromHttpApiData, ToParamSchema)

type Coords = ((Double, Double), (Double, Double))

bboxToCoords :: Text -> Either Text ((Double, Double), (Double, Double))
bboxToCoords bbox = first toText parseBBox
  where
    parseBBox = mapM (fmap fst . double) (splitOn "," bbox) >>= toTuple
    toTuple [x1, y1, x2, y2] = Right ((x1, y1), (x2, y2))
    toTuple _ = Left "wrong bbox"

newtype ScopeDescription = ScopeDescription {value :: Text}
  deriving newtype (Show, FromJSON, ToJSON, ToSchema)

newtype ScopeName = ScopeName {value :: Text}
  deriving newtype (Show, FromJSON, ToJSON, ToSchema)
