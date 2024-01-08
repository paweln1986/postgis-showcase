module Leaflet.Map
  ( Map
  , getBounds
  , getZoom
  , map
  , updateGeoJSON
  , fitBounds
  , addGeoJSON
  ) where

import Prelude
import Data.Argonaut.Core (Json)
import Effect (Effect)

foreign import data Map :: Type

foreign import map :: String -> (String -> Int -> Effect Unit) -> (Json -> Effect Unit) -> Effect Unit -> Effect Map

foreign import updateGeoJSON :: Map -> Json -> Effect Unit

foreign import addGeoJSON :: Map -> Json -> Effect Unit

foreign import getZoom :: Map -> Effect Int

foreign import getBounds :: Map -> Effect String

foreign import fitBounds :: Map -> Effect Unit
