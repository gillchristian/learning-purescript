module Route where

import Prelude hiding ((/))
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex (RouteDuplex', root)
import Routing.Duplex.Generic.Syntax ((/))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

data Route
  = Home
  | Login
  | Dashboard

derive instance genericRoute :: Generic Route _
derive instance eqRoute :: Eq Route
derive instance ordRoute :: Ord Route

instance showRoute :: Show Route where
  show = genericShow

routeCodec :: RouteDuplex' Route
routeCodec = root $ sum
  { "Home": noArgs
  , "Login": "login" / noArgs
  , "Dashboard": "dashboard" / noArgs
  }
