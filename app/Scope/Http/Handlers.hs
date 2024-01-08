module Scope.Http.Handlers where

import Effectful (Eff, type (:>))
import Scope.Http.Api (CreateScopeApi, GetAllScopes, ScopeApi)
import Scope.Http.Payloads (CreateScopeRequest (..))
import Scope.Registry (ScopeRegistry, getAllScopes, saveScope)
import Servant (ServerT, type (:<|>) ((:<|>)))
import Utils.Data (Polygon (value))

scopeApiHandler :: (ScopeRegistry :> es) => ServerT ScopeApi (Eff es)
scopeApiHandler = createScopeHandler :<|> getAllScopesHandler

getAllScopesHandler :: (ScopeRegistry :> es) => ServerT GetAllScopes (Eff es)
getAllScopesHandler = getAllScopes

createScopeHandler :: (ScopeRegistry :> es) => ServerT CreateScopeApi (Eff es)
createScopeHandler request = do
  let geom = request.polygon.value
  saveScope request.name request.description geom
