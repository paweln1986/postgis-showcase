module App.CreateScopePage
  ( _createScopeSlot
  , component
  ) where

import Prelude
import Affjax.RequestBody (json)
import Affjax.ResponseFormat as ResponseFormat
import Affjax.Web (printError)
import Affjax.Web as Affjax
import Data.Argonaut (encodeJson)
import Data.Argonaut.Core (Json)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.String.NonEmpty (NonEmptyString, fromString, toString)
import Effect.Aff (launchAff_)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Leaflet.Map (Map, updateGeoJSON)
import Leaflet.Map as Leaflet
import Type.Prelude (Proxy(..))

type Scope
  = { polygon :: Maybe Json
    , name :: Maybe NonEmptyString
    , description :: Maybe String
    }

type State
  = { count :: Int
    , submittingScope :: Boolean
    , scope :: Scope
    , showCreateScopeSuccess :: Boolean
    , map :: Maybe Map
    }

data Action
  = Init
  | PolygonUpdated Json
  | PolygonRemoved
  | ScopeNameUpdated String
  | ScopeSubmitted
  | UpdateGeoJSON Json

_createScopeSlot = Proxy :: Proxy "createScopeSlot"

emptyScope :: Scope
emptyScope = { polygon: Nothing, name: Nothing, description: Nothing }

component :: forall q i o m. MonadEffect m => MonadAff m => H.Component q i o m
component =
  H.mkComponent
    { initialState: \_ -> { map: Nothing, count: 0, submittingScope: false, scope: emptyScope, showCreateScopeSuccess: false }
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction, initialize = Just Init }
    }

render :: forall cs m. State -> H.ComponentHTML Action cs m
render state =
  HH.div_
    [ HH.div_ [ HH.h1_ [ HH.text "Create scope:" ] ]
    , HH.div [ HP.class_ (cls "container-fluid") ]
        [ HH.div [ HP.class_ (cls "row") ]
            [ HH.div [ HP.class_ (cls "col col-lg-2") ]
                [ HH.div [ HP.class_ (cls "mb-3") ]
                    [ HH.label [ HP.for "scopeNameInput", HP.class_ (cls "form-label") ] [ HH.text "Scope name:" ]
                    , HH.input [ HE.onValueInput ScopeNameUpdated, HP.id "scopeNameInput", HP.class_ (cls nameClass), HP.placeholder "Name of new scope..." ]
                    ]
                , HH.div [ HP.class_ (cls "mb-3") ]
                    [ HH.label [ HP.for "scopeDescriptionInput", HP.class_ (cls "form-label") ] [ HH.text "Scope description:" ]
                    , HH.textarea [ HP.rows 5, HP.id "scopeDescriptionInput", HP.class_ (cls "form-control"), HP.placeholder "Description of new scope..." ]
                    ]
                , HH.div [ HP.class_ (cls "container") ]
                    [ HH.div [ HP.class_ (cls "row") ]
                        [ HH.div [ HP.class_ (cls "col") ]
                            [ HH.button [ HE.onClick \_ -> ScopeSubmitted, HP.class_ (cls "btn btn-warning btn-lg"), disableWhenScopeIsSubmitted ]
                                [ HH.span_ [ HH.text " Create scope" ] ]
                            ]
                        , HH.div [ HP.class_ (cls "col") ]
                            [ if state.showCreateScopeSuccess then
                                HH.div [ HP.class_ (cls "alert alert-success") ]
                                  [ HH.text $ (fromMaybe "unknown" (toString <$> state.scope.name)) <> " scope created!" ]
                              else
                                HH.span_ []
                            ]
                        ]
                    ]
                ]
            , HH.div [ HP.class_ (cls "col") ] [ HH.div [ HP.id "map1" ] [] ]
            ]
        ]
    ]
  where
  disableWhenScopeIsSubmitted = HP.disabled (can'tCreateScope state.scope || state.submittingScope)

  nameClass = "form-control" <> " " <> if isJust state.scope.name then "" else "border-danger"

handleAction :: forall cs o m. MonadEffect m => MonadAff m => Action → H.HalogenM State Action cs o m Unit
handleAction = case _ of
  ScopeSubmitted -> do
    state <- H.get
    let
      a = encodeJson state.scope
    void $ H.liftAff $ Affjax.post_ "/api/create-scope" (Just $ json a)
    H.modify_ \st -> st { submittingScope = false, showCreateScopeSuccess = true }
  PolygonRemoved -> H.modify_ \st -> st { scope { polygon = Nothing } }
  ScopeNameUpdated a -> H.modify_ \st -> st { scope { name = fromString a } }
  PolygonUpdated p -> H.modify_ \st -> st { scope { polygon = Just p } }
  UpdateGeoJSON json -> do
    state <- H.get
    case state.map of
      Just map -> liftEffect $ updateGeoJSON map json
      Nothing -> log "map is not initialized"
  Init -> do
    { emitter, listener } <- H.liftEffect HS.create
    _ <- H.subscribe emitter
    map <-
      H.liftEffect
        $ Leaflet.map
            "map1"
            (\bbox zoom -> updateMapData bbox zoom listener)
            (\a -> HS.notify listener (PolygonUpdated a))
            (HS.notify listener PolygonRemoved)
    H.modify_ \st -> st { map = Just map }
  where
  updateMapData bbox zoom listener =
    launchAff_ do
      geojson <- Affjax.get ResponseFormat.json ("/api/antenna?bbox=" <> bbox <> "&zoom=" <> show zoom)
      case geojson of
        Right a -> H.liftEffect $ HS.notify listener $ UpdateGeoJSON a.body
        Left err -> log $ printError err

cls :: String -> ClassName
cls = ClassName

canCreateScope :: Scope -> Boolean
canCreateScope scope = isJust scope.polygon && isJust scope.name

can'tCreateScope :: Scope -> Boolean
can'tCreateScope scope = not (canCreateScope scope)
