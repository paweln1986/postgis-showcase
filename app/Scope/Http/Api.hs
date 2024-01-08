{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}

module Scope.Http.Api where

import Data.Vector (Vector)
import Scope.Http.Payloads (CreateScopeRequest (..), Scope)
import Servant
  ( Get,
    JSON,
    Post,
    ReqBody,
    Summary,
    type (:<|>),
    type (:>),
  )
import Utils.Data (ScopeId)

type CreateScopeApi =
  Summary "Create scope with given area"
    :> "create-scope"
    :> ReqBody '[JSON] CreateScopeRequest
    :> Post '[JSON] ScopeId

type GetAllScopes =
  Summary "Fetch all available scopes"
    :> "scopes"
    :> Get '[JSON] (Vector Scope)

type ScopeApi = CreateScopeApi :<|> GetAllScopes
