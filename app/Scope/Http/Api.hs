{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}

module Scope.Http.Api where

import Data.Vector (Vector)
import Effectful (Eff)
import Effectful.Error.Static (Error)
import Scope.Data (Polygon (unGeoPoly), ScopeId)
import Scope.Http.Payloads (CreateScopeRequest (..), Scope)
import Scope.Registry (ScopeRegistry, getAllScopes, saveScope)
import Servant hiding (throwError)

type CreateScopeApi = Summary "Create scope with given area" :> "create-scope" :> ReqBody '[JSON] CreateScopeRequest :> Post '[JSON] ScopeId
type GetAllScopes = Summary "Fetch all available scopes" :> "scopes" :> Get '[JSON] (Vector Scope)

type ScopeApi = CreateScopeApi :<|> GetAllScopes

scopeApiHandler :: ServerT ScopeApi (Eff (ScopeRegistry : Error ServerError : es))
scopeApiHandler = createScopeHandler :<|> getAllScopesHandler

getAllScopesHandler :: ServerT GetAllScopes (Eff (ScopeRegistry : es))
getAllScopesHandler = getAllScopes

createScopeHandler :: ServerT CreateScopeApi (Eff (ScopeRegistry : Error ServerError : es))
createScopeHandler request = do
    let geom = unGeoPoly request.polygon
    saveScope request.name request.description geom
