module Leaflet.Map where

import Prelude

import Data.Argonaut.Core (Json)
import Effect (Effect)

foreign import data Map :: Type

foreign import map :: String -> (Json -> Effect Unit) -> Effect Unit -> Effect Map