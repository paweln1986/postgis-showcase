module Antenna.Http.Payloads where

import Data.Geospatial (GeoFeature, GeoPoint)
import Data.OpenApi (ToParamSchema, ToSchema (declareNamedSchema))
import Deriving.Aeson
import Servant (FromHttpApiData (parseQueryParam))
import System.Random (Uniform)

newtype AntennaId = AntennaId {unAntennaId :: Int64} deriving newtype (Show, ToJSON, FromJSON, ToSchema)

newtype Direction = Direction {unDirection :: Int16} deriving newtype (Show, ToJSON, FromJSON, ToSchema)

newtype Point = Point {unPoint :: GeoPoint} deriving newtype (Show, FromJSON, ToJSON)

instance ToSchema Point where
  declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy String)

data AntennaType = GSM | WCDMA | LTE | FiveG deriving (Show, Read, Generic, Bounded, Enum)

instance Uniform AntennaType

instance FromJSON AntennaType

instance ToJSON AntennaType

instance ToSchema AntennaType

instance ToParamSchema AntennaType

instance FromHttpApiData AntennaType where
  parseQueryParam = toAntennaType

toAntennaType :: Text -> Either Text AntennaType
toAntennaType = readEither . toString

data Antenna = Antenna
  { id :: AntennaId,
    direction :: Direction,
    location :: Point,
    antenna_type :: AntennaType
  }
  deriving (Show, Generic)

data AntennaProperties
  = SingleAntenna
      { antenna_direction :: Direction,
        antenna_type :: AntennaType
      }
  | ClusterAntenna
      { size :: Int,
        radius :: Double
      }
  deriving stock (Show, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[TagSingleConstructors, OmitNothingFields, FieldLabelModifier '[]] AntennaProperties

instance ToSchema AntennaProperties

newtype AntennaGeoJSON = AntennaGeoJSON
  { value :: GeoFeature AntennaProperties
  }
  deriving newtype (ToJSON, FromJSON, Show)

instance FromJSON Antenna

instance ToJSON Antenna

instance ToSchema Antenna

instance ToSchema AntennaGeoJSON where
  declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy String)
