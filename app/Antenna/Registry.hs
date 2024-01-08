{-# LANGUAGE TemplateHaskell #-}

module Antenna.Registry where

import Antenna.Http.Payloads (AntennaProperties, AntennaType, Direction (..), Point (..))
import Antenna.Queries (insertAntenna, selectAntennasInsideBBox, selectAntennasInsideScope, selectClusteredAntennasBBox)
import Colog (Message)
import Data.Geospatial (GeoFeature, GeoPoint (..), GeoPositionWithoutCRS (..), PointXY (..))
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Database.Connection (DatabaseConnection, runStatement)
import Effectful (Eff, Effect, IOE, type (:>))
import Effectful.Crypto.RNG (CryptoRNG (random), RNG, randomR)
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.TH (makeEffect)
import Utils.Data (Coords, ScopeId (..))
import Utils.Log (Log, logInfo)

data AntennaRegistry :: Effect where
  GetAllForScope :: ScopeId -> NonEmpty AntennaType -> AntennaRegistry m (Vector (GeoFeature AntennaProperties))
  GetAllForBBox :: Coords -> Int -> AntennaRegistry m (Vector (GeoFeature AntennaProperties))
  CreateAntenna :: Direction -> Point -> AntennaType -> AntennaRegistry m ()
  CreateAntennas :: Vector (Direction, Point, AntennaType) -> AntennaRegistry m ()

makeEffect ''AntennaRegistry

type AntenaRegistryHandler es a =
  (DatabaseConnection :> es, Log Message :> es, IOE :> es) =>
  Eff (AntennaRegistry : es) a ->
  Eff es a

handleAntennaRegistry :: AntenaRegistryHandler es a
handleAntennaRegistry = interpret $ \_ -> \case
  GetAllForScope scopeId antennaTypes -> do
    logInfo $ "fetch antena inside scope: " <> show scopeId
    runStatement $ selectAntennasInsideScope scopeId (fromList $ toList antennaTypes)
  GetAllForBBox coords zoom ->
    case zoom of
      _ | zoom > 9 -> do
        logInfo $ "fetch antena inside bbox: " <> show coords <> " and zoom " <> show zoom
        runStatement $ selectAntennasInsideBBox coords
      _ -> do
        logInfo $ "fetch clustered antena inside bbox: " <> show coords <> " and zoom " <> show zoom
        runStatement $ selectClusteredAntennasBBox coords (fromIntegral (abs (31 - zoom)))
  CreateAntenna direction point antennaType -> do
    logInfo $ "creating antenna: " <> show (direction, antennaType, point)
    runStatement $ insertAntenna $ Vector.singleton (direction, point.unPoint, antennaType)
  CreateAntennas antennas -> do
    logInfo $ "creating antennas: " <> show (Vector.length antennas)
    runStatement $ insertAntenna $ fmap (\(a, b, c) -> (a, b.unPoint, c)) antennas

createRandomAntennas ::
  (RNG :> es, AntennaRegistry :> es) =>
  Int ->
  Coords ->
  Eff es ()
createRandomAntennas noOfAntennas bbox = do
  antennas <- generateRandomAntennas noOfAntennas bbox Vector.empty
  createAntennas antennas

generateRandomAntennas :: (RNG :> es, AntennaRegistry :> es) => Int -> Coords -> Vector (Direction, Point, AntennaType) -> Eff es (Vector (Direction, Point, AntennaType))
generateRandomAntennas noOfAntennas bbox acc
  | noOfAntennas <= 0 = pure acc
  | otherwise = do
      antennas <- generateRandomAntennaForBBox bbox
      generateRandomAntennas (noOfAntennas - 1) bbox (Vector.concat [acc, antennas])

generateRandomAntennaForBBox ::
  forall es.
  (RNG :> es, AntennaRegistry :> es) =>
  Coords ->
  Eff es (Vector (Direction, Point, AntennaType))
generateRandomAntennaForBBox ((x1, y1), (x2, y2)) = do
  randomPoint <- PointXY <$> randomR (x1, x2) <*> randomR (y1, y2)
  let point = Point . GeoPoint . GeoPointXY $ randomPoint
  noOfAntennas <- randomR (1 :: Int16, 6)
  let antennaStep = 360 `div` noOfAntennas
  randomAntennaType <- random
  antennas <- traverse ((\x -> Direction <$> randomR (x - antennaStep, x)) . (* antennaStep)) [1 .. noOfAntennas]
  Vector.fromList <$> generate point randomAntennaType [] antennas
  where
    generate :: Point -> AntennaType -> [(Direction, Point, AntennaType)] -> [Direction] -> Eff es [(Direction, Point, AntennaType)]
    generate _ _ acc [] = pure acc
    generate point randomAntennaType acc (direction : xs) = do
      generate point randomAntennaType ((direction, point, randomAntennaType) : acc) xs
