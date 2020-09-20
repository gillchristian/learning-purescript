module App.Router where

import Prelude
import Api as Api
import App.LoginForm as LoginForm
import App.Timer as Timer
import AppM (SessionEnv)
import Control.Monad.Reader.Trans (class MonadAsk)
import Data.Either (hush)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import HOC.Connect as Connect
import Halogen as H
import Halogen.HTML as HH
import Navigate (class Navigate, navigate)
import Route (Route(..))
import Route as Route
import Routing.Duplex as RD
import Routing.Hash (getHash)
import Util (OpaqueSlot)

type State
  = { route :: Maybe Route.Route
    , currentUser :: Maybe Api.User
    }

data Query a
  = Navigate Route.Route a

data Action
  = Initialize
  | Receive { currentUser :: Maybe Api.User }

type ChildSlots
  = ( home :: OpaqueSlot Unit
    , login :: OpaqueSlot Unit
    , dashboard :: OpaqueSlot Unit
    )

component ::
  forall m r.
  MonadAff m =>
  Navigate m =>
  MonadAsk { sessionEnv :: SessionEnv | r } m =>
  H.Component HH.HTML Query {} Void m
component =
  Connect.component
    $ H.mkComponent
        { initialState: \{ currentUser } -> { route: Nothing, currentUser }
        , render
        , eval:
            H.mkEval
              $ H.defaultEval
                  { handleQuery = handleQuery
                  , handleAction = handleAction
                  , receive = Just <<< Receive
                  , initialize = Just Initialize
                  }
        }
  where
  handleAction :: Action -> H.HalogenM State Action ChildSlots Void m Unit
  handleAction = case _ of
    Initialize -> do
      initialRoute <- hush <<< (RD.parse Route.routeCodec) <$> liftEffect getHash
      navigate $ fromMaybe Route.Home initialRoute
    Receive { currentUser } -> H.modify_ _ { currentUser = currentUser }

  handleQuery :: forall a. Query a -> H.HalogenM State Action ChildSlots Void m (Maybe a)
  handleQuery = case _ of
    Navigate dest a -> do
      { route, currentUser } <- H.get
      when (route /= Just dest) do
        {-- H.modify_ _ { route = Just dest } --}
        -- TODO use navigate instead ???
        case isJust currentUser && dest == Route.Login of
          false -> H.modify_ _ { route = Just dest }
          _ -> pure unit
      pure (Just a)

  authorize :: Maybe Api.User -> H.ComponentHTML Action ChildSlots m -> H.ComponentHTML Action ChildSlots m
  authorize mbProfile html = case mbProfile of
    Nothing -> HH.slot (SProxy :: _ "login") unit LoginForm.component { redirect: false } absurd
    Just _ -> html

  render :: State -> H.ComponentHTML Action ChildSlots m
  render { route, currentUser } = case route of
    Just r -> case r of
      Home ->
        HH.div_
          [ HH.div_ [ HH.text "Home" ]
          , HH.slot (SProxy :: _ "home") unit Timer.component {} absurd
          ]
      Login -> HH.slot (SProxy :: _ "login") unit LoginForm.component { redirect: true } absurd
      Dashboard ->
        HH.div_
          [ HH.div_ [ HH.text "Dashboard" ]
          , HH.slot (SProxy :: _ "dashboard") unit Timer.component unit absurd
          ]
          # authorize currentUser
    Nothing -> HH.div_ [ HH.text "Oh no! That page wasn't found." ]
