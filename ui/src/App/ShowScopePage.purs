module App.ShowScopePage
  ( _showScopeSlot
  , component
  ) where

import Prelude
import Affjax.ResponseFormat as ResponseFormat
import Affjax.Web (printError)
import Affjax.Web as Affjax
import Data.Argonaut (Json, decodeJson, printJsonDecodeError)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Effect.Exception (throw)
import Halogen (ClassName(..), PropName(..))
import Halogen as H
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Events (onValueInput) as HP
import Halogen.HTML.Properties (prop)
import Halogen.HTML.Properties (class_, id, value) as HP
import Leaflet.Map (addGeoJSON, fitBounds, updateGeoJSON)
import Leaflet.Map as Leaflet
import Type.Prelude (Proxy(..))

type Scope
  = { polygon :: Json
    , scope_name :: String
    , scope_description :: Maybe String
    , scope_id :: Int
    }

type State
  = { scopes :: Array Scope
    , selectedScopeId :: Maybe Scope
    , map :: Maybe Leaflet.Map
    , selectedAntennatype :: Maybe String
    }

data Action
  = Init
  | ScopeSelected Scope
  | FilterSelected String

_showScopeSlot = Proxy :: Proxy "showScopeSlot"

component :: forall q i o m. MonadEffect m => MonadAff m => H.Component q i o m
component =
  H.mkComponent
    { initialState: \_ -> { scopes: [], selectedScopeId: Nothing, map: Nothing, selectedAntennatype: Nothing }
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction, initialize = Just Init }
    }

render :: forall cs m. State -> H.ComponentHTML Action cs m
render state =
  HH.div []
    [ HH.div_ [ HH.h1_ [ HH.text "Show scope:" ] ]
    , HH.div [ HP.class_ (cls "container-fluid") ]
        [ HH.div [ HP.class_ (cls "row") ]
            [ HH.div [ HP.class_ (cls "col col-lg-2") ]
                [ HH.select [ HP.class_ (cls "form-select"), size 20 ] renderScopes
                , HH.select [ HP.onValueInput FilterSelected, HP.class_ (cls "form-select"), size 10 ]
                    [ HH.option [ HP.value "All" ] [ HH.text "All" ]
                    , HH.option [ HP.value "GSM" ] [ HH.text "GSM" ]
                    , HH.option [ HP.value "WCDMA" ] [ HH.text "WCDMA" ]
                    , HH.option [ HP.value "LTE" ] [ HH.text "LTE" ]
                    , HH.option [ HP.value "FiveG" ] [ HH.text "FiveG" ]
                    ]
                ]
            , HH.div [ HP.class_ (cls "col") ] [ HH.div [ HP.id "scope_map" ] [] ]
            ]
        ]
    ]
  where
  renderScopes = (renderScope <$> state.scopes)

  renderScope scope = HH.option [ onClick (\_ -> ScopeSelected scope), HP.value $ show scope.scope_id ] [ HH.text scope.scope_name ]

handleAction :: forall cs o m. MonadEffect m => MonadAff m => Action → H.HalogenM State Action cs o m Unit
handleAction = case _ of
  FilterSelected "All" -> do
    H.modify_ \st -> st { selectedAntennatype = Nothing }
    state <- H.get
    case state.selectedScopeId of
      Just scope -> updateMap scope
      Nothing -> log "no scope selected"
  FilterSelected rest -> do
    H.modify_ \st -> st { selectedAntennatype = Just rest }
    state <- H.get
    case state.selectedScopeId of
      Just scope -> updateMap scope
      Nothing -> log "no scope selected"
  ScopeSelected scope -> do
    log $ "scope selected " <> show scope.scope_id
    H.modify_ \st -> st { selectedScopeId = Just scope }
    updateMap scope
  Init -> do
    scopesResponse <- H.liftAff $ Affjax.get ResponseFormat.json "/api/scopes"
    sc <- case scopesResponse of
      Left error -> H.liftEffect $ throw $ "could not load scopes" <> printError error
      Right out -> case decodeJson out.body of
        Right a -> pure a
        Left msg -> H.liftEffect $ throw $ printJsonDecodeError msg
    H.modify_ \st -> st { scopes = sc }
    map <- H.liftEffect $ Leaflet.map "scope_map" (\_ _ -> pure unit) (\_ -> pure unit) (pure unit)
    H.modify_ \st -> st { map = Just map }
  where
  updateMap scope = do
    state <- H.get
    case state.map of
      Just map -> do
        H.liftEffect $ updateGeoJSON map scope.polygon
        H.liftEffect $ fitBounds map
        geojson <- getScope scope.scope_id
        case geojson of
          Right res -> H.liftEffect $ addGeoJSON map res.body
          Left err -> H.liftEffect $ throw $ "could not load scopes" <> printError err
      Nothing -> log "map not initialized"

  getScope scopeId = do
    state <- H.get
    let
      url = case state.selectedAntennatype of
        Nothing -> "/api/scopes/" <> show scopeId <> "/antenna"
        Just select -> ("/api/scopes/" <> show scopeId <> "/antenna?band=" <> select)
    H.liftAff $ Affjax.get ResponseFormat.json url

cls :: String -> ClassName
cls = ClassName

size :: forall r i. Int -> IProp ( size :: Int | r ) i
size = prop (PropName "size")
