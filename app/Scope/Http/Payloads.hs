{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}

module Scope.Http.Payloads where

import Data.OpenApi (ToSchema)
import Deriving.Aeson
  ( CamelToSnake,
    CustomJSON (CustomJSON),
    FieldLabelModifier,
    FromJSON,
    OmitNothingFields,
    ToJSON,
  )
import Utils.Data (Polygon, ScopeDescription, ScopeId, ScopeName)

data CreateScopeRequest = CreateScopeRequest {name :: ScopeName, description :: Maybe ScopeDescription, polygon :: Polygon}
  deriving (Generic, Show)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[OmitNothingFields, FieldLabelModifier '[CamelToSnake]] CreateScopeRequest

instance ToSchema CreateScopeRequest

data Scope = Scope
  { scopeId :: ScopeId,
    scopeName :: ScopeName,
    scopeDescription :: Maybe ScopeDescription,
    polygon :: Polygon
  }
  deriving (Generic, Show)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[OmitNothingFields, FieldLabelModifier '[CamelToSnake]] Scope

instance ToSchema Scope
