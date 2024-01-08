{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}

module Antenna.Http.Api where

import Antenna.Http.Payloads (AntennaGeoJSON, AntennaType)
import Data.Vector (Vector)
import Servant (Capture, Get, JSON, PostNoContent, QueryParam, QueryParam', ReqBody, Required, Strict, Summary, (:<|>), type (:>), QueryParams)
import Utils.Data (BBox, ScopeId)

type GetAntennaInsideScope =
  Summary "Fetch all antenna inside scope"
    :> "scopes"
    :> Capture "scopeId" ScopeId
    :> "antenna"
    :> QueryParams "band" AntennaType
    :> Get '[JSON] (Vector AntennaGeoJSON)

type GetAntennaGeoJSON =
  Summary "Fetch all antenna by bbox"
    :> "antenna"
    :> QueryParam' '[Required, Strict] "bbox" BBox
    :> QueryParam' '[Required, Strict] "zoom" Int
    :> Get '[JSON] (Vector AntennaGeoJSON)

type CreateRandomAntennas =
  Summary "Create random atennas for bounding box"
    :> "antenna"
    :> "random"
    :> QueryParam "noOfAntennas" Int
    :> ReqBody '[JSON] BBox
    :> PostNoContent

type AntennaApi = GetAntennaInsideScope :<|> CreateRandomAntennas :<|> GetAntennaGeoJSON
