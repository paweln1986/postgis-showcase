module App.AppMainPage
  ( component, Query(..)
  ) where

import Prelude
import App.CreateScopePage as CreateScopePage
import App.Navigate (class Navigate, navigate)
import App.Route (Route(..))
import App.ShowScopePage as ShowScopePage
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.Event.Event (preventDefault)
import Web.UIEvent.MouseEvent (MouseEvent, toEvent)

type State
  = { route :: Maybe Route
    }

data Action
  = Init
  | GoTo Route MouseEvent

data Query a
  = Navigate Route a

type Slots
  = ( createScopeSlot :: forall query. H.Slot query Void Unit
    , showScopeSlot :: forall query. H.Slot query Void Unit
    )

handleQuery :: forall a o m. Query a -> H.HalogenM State Action Slots o m (Maybe a)
handleQuery = case _ of
  Navigate route a -> do
    mRoute <- H.gets _.route
    when (mRoute /= Just route)
      $ H.modify_ _ { route = Just route }
    pure (Just a)

component ::
  forall i o m.
  MonadEffect m =>
  Navigate m => MonadAff m => H.Component Query i o m
component =
  H.mkComponent
    { initialState: \_ -> { route: Just Home }
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction, handleQuery = handleQuery }
    }

render :: forall m. MonadEffect m => MonadAff m => State -> H.ComponentHTML Action Slots m
render state =
  HH.div_
    [ HH.nav [ HP.class_ (ClassName "navbar navbar-dark bg-dark navbar-expand-lg") ]
        [ HH.div [ HP.class_ (ClassName "container-fluid") ]
            [ HH.a [ HP.class_ (ClassName "navbar-brand") ] [ HH.text "ScopeManager" ]
            , HH.ul [ HP.class_ (ClassName "navbar-nav me-auto mb-2 mb-lg-0") ]
                [ HH.li [ HP.class_ (ClassName "nav-item") ]
                    [ HH.a [ HP.class_ (ClassName "nav-link"), HP.href "#", HE.onClick (GoTo CreateScope) ] [ HH.text "Create Scope" ]
                    ]
                , HH.li [ HP.class_ (ClassName "nav-item") ]
                    [ HH.a [ HP.class_ (ClassName "nav-link"), HP.href "#", HE.onClick (GoTo ShowScopes) ] [ HH.text "Show Scopes" ]
                    ]
                ]
            ]
        ]
    , HH.div [ HP.class_ (ClassName "main") ]
        [ case state.route of
            Just ShowScopes -> HH.slot_ ShowScopePage._showScopeSlot unit ShowScopePage.component unit
            Just CreateScope -> HH.slot_ CreateScopePage._createScopeSlot unit CreateScopePage.component unit
            Just Home -> HH.h1_ [ HH.text "Scope manager" ]
            Nothing -> HH.h1_ [ HH.text "Oh no! That page wasn't found" ]
        ]
    ]

handleAction :: forall cs o m. MonadEffect m => Navigate m => Action → H.HalogenM State Action cs o m Unit
handleAction = case _ of
  Init -> pure unit
  GoTo route e -> do
    log $ show route
    liftEffect $ preventDefault (toEvent e)
    mRoute <- H.gets _.route
    when (mRoute /= Just route) $ navigate route
