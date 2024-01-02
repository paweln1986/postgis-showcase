module App.AppMainPage
  ( component
  ) where

import Prelude

import App.CreateScopePage as CreateScopePage
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

type State =
  {
  }

data Action = Init

type Slots = (createScopeSlot :: forall query. H.Slot query Void Unit)

component :: forall q i o m. MonadEffect m => MonadAff m => H.Component q i o m
component =
  H.mkComponent
    { initialState: \_ -> {}
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }

render :: forall m. MonadEffect m => MonadAff m => State -> H.ComponentHTML Action Slots m
render _ =
  HH.div_
    [ HH.nav [ HP.class_ (ClassName "navbar navbar-dark bg-dark navbar-expand-lg") ]
        [ HH.div [ HP.class_ (ClassName "container-fluid") ]
            [ HH.a [ HP.class_ (ClassName "navbar-brand") ] [ HH.text "ScopeManager" ]
            , HH.ul [ HP.class_ (ClassName "navbar-nav me-auto mb-2 mb-lg-0") ]
                [ HH.li [ HP.class_ (ClassName "nav-item") ]
                    [ HH.a [ HP.class_ (ClassName "nav-link"), HP.href "#" ] [ HH.text "Create Scope" ]
                    ]
                , HH.li [ HP.class_ (ClassName "nav-item") ]
                    [ HH.a [ HP.class_ (ClassName "nav-link"), HP.href "#" ] [ HH.text "Create Antenna" ]
                    ]
                , HH.li [ HP.class_ (ClassName "nav-item") ]
                    [ HH.a [ HP.class_ (ClassName "nav-link"), HP.href "#" ] [ HH.text "Show Scopes" ]
                    ]
                ]
            ]
        ]
    , HH.div_
        [ HH.slot_ CreateScopePage._createScopeSlot unit CreateScopePage.component unit
        ]
    ]

handleAction :: forall cs o m. MonadEffect m => Action → H.HalogenM State Action cs o m Unit
handleAction _ = pure unit