module App.Navigate where

import Prelude
import App.Route (Route)
import Halogen (HalogenM, lift)

class
  Monad m <= Navigate m where
  navigate :: Route -> m Unit

instance navigateHalogenM :: Navigate m => Navigate (HalogenM state action slots msg m) where
  navigate = lift <<< navigate
