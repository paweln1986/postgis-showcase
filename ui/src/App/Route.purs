module App.Route where

import Prelude hiding ((/))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Routing.Duplex (RouteDuplex', root)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))

data Route
  = CreateScope
  | ShowScopes
  | Home

instance showMyADT :: Show Route where
  show = genericShow

derive instance genericRoute :: Generic Route _

derive instance eqRoute :: Eq Route

derive instance ordRoute :: Ord Route

routeCodec :: RouteDuplex' Route
routeCodec =
  root
    $ sum
        { "CreateScope": "create-scope" / noArgs
        , "ShowScopes": "show-scopes" / noArgs
        , "Home": noArgs
        }
