module Antenna.Http.Handlers where

import Antenna.Http.Api (AntennaApi, CreateRandomAntennas, GetAntennaGeoJSON, GetAntennaInsideScope)
import Antenna.Http.Payloads (AntennaGeoJSON (..))
import Antenna.Registry (AntennaRegistry, createRandomAntennas, getAllForBBox, getAllForScope)
import Effectful (Eff, type (:>))
import Effectful.Crypto.RNG (RNG)
import Effectful.Error.Static (throwError)
import Effectful.Error.Static qualified as Static
import Servant (NoContent (NoContent), ServerError (errBody), ServerT, err400, type (:<|>) ((:<|>)))
import Utils.Data (BBox (..), Coords, bboxToCoords)

antennaApiHandler ::
  (AntennaRegistry :> es, RNG :> es, Static.Error ServerError :> es) =>
  ServerT AntennaApi (Eff es)
antennaApiHandler = getAllScopesHandler :<|> createRandomAntennasHandler :<|> getAntennaGeoJSONHandler

getAllScopesHandler :: (AntennaRegistry :> es) => ServerT GetAntennaInsideScope (Eff es)
getAllScopesHandler scopeId antennaType = coerce $ getAllForScope scopeId filters
  where
    filters = fromMaybe allAntennaTypes $ nonEmpty antennaType
    allAntennaTypes = fromList $ enumFrom (toEnum 0)

getAntennaGeoJSONHandler ::
  (AntennaRegistry :> es, Static.Error ServerError :> es) =>
  ServerT GetAntennaGeoJSON (Eff es)
getAntennaGeoJSONHandler bbox zoom = do
  coords <- parseBBoxParam bbox
  coerce $ getAllForBBox coords zoom

createRandomAntennasHandler ::
  (RNG :> es, AntennaRegistry :> es, Static.Error ServerError :> es) =>
  ServerT CreateRandomAntennas (Eff es)
createRandomAntennasHandler noOfAntennasParam bbox = do
  coords <- parseBBoxParam bbox
  let noOfAntennas = fromMaybe 100 noOfAntennasParam
  createRandomAntennas noOfAntennas coords
  pure NoContent

parseBBoxParam :: (Static.Error ServerError :> es) => BBox -> Eff es Coords
parseBBoxParam bbox = do
  let parsedCoords = bboxToCoords bbox.value
  handleError $ first (\x -> err400 {errBody = encodeUtf8 x}) parsedCoords

handleError :: (Static.Error e :> es) => Either e a -> Eff es a
handleError (Right a) = pure a
handleError (Left err) = throwError err