module Main where

import Prelude
import App.AppMainPage as AppMainPage
import App.Route (routeCodec)
import AppM (runAppM)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Halogen (liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Routing.Duplex (parse)
import Routing.PushState (makeInterface, matchesWith)

main :: Effect Unit
main = do
  nav <- makeInterface
  HA.runHalogenAff do
    body <- HA.awaitBody
    let
      env = { navInterface: nav }
    let
      component = H.hoist (runAppM env) AppMainPage.component
    halogenIO <- runUI component unit body
    void $ liftEffect
      $ matchesWith (parse routeCodec)
          ( \old new ->
              when (old /= Just new)
                $ launchAff_ do
                    _response <- halogenIO.query $ H.mkTell $ AppMainPage.Navigate new
                    pure unit
          )
          nav
