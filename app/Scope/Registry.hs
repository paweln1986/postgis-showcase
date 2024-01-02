{-# LANGUAGE TemplateHaskell #-}

module Scope.Registry where

import Data.Vector (Vector)
import Effectful (Eff, Effect, type (:>), IOE)
import Effectful.Dispatch.Dynamic (interpret)

import Colog (Message)
import Data.Geospatial (GeoPolygon)
import Database.Connection (DatabaseConnection, runStatement)
import Effectful.TH (makeEffect)
import Scope.Data (ScopeDescription, ScopeId, ScopeName)
import Scope.Queries (insertScope, selectAllScope)
import Utils.Log (Log, logInfo)
import Scope.Http.Payloads ( Scope )

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

newtype ScopeRegistryError = ScopeRegistryError
    { message :: Text
    }
    deriving stock (Show)

instance Exception ScopeRegistryError