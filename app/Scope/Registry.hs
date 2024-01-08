{-# LANGUAGE TemplateHaskell #-}

module Scope.Registry where

import Colog (Message)
import Data.Geospatial (GeoPolygon)
import Data.Vector (Vector)
import Database.Connection (DatabaseConnection, runStatement)
import Effectful (Eff, Effect, IOE, type (:>))
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.TH (makeEffect)
import Scope.Http.Payloads (Scope)
import Scope.Queries (insertScope, selectAllScope)
import Utils.Data (ScopeDescription, ScopeId, ScopeName)
import Utils.Log (Log, logInfo)

data ScopeRegistry :: Effect where
  GetAllScopes :: ScopeRegistry m (Vector Scope)
  SaveScope :: ScopeName -> Maybe ScopeDescription -> GeoPolygon -> ScopeRegistry m ScopeId

makeEffect ''ScopeRegistry

handleScopeRegistry :: (DatabaseConnection :> es, Log Message :> es, IOE :> es) => Eff (ScopeRegistry : es) a -> Eff es a
handleScopeRegistry = interpret $ \_ -> \case
  GetAllScopes -> do
    logInfo "fetch all available scopes"
    runStatement selectAllScope
  SaveScope newScope scopeDescription polygon -> do
    logInfo $ "save new scope: " <> show newScope
    runStatement $ insertScope (newScope, scopeDescription, polygon)
