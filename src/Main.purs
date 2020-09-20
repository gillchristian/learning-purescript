module Main where

import Prelude
import Api as Api
import App.Router as Router
import AppM (runAppM, Env, SessionEnv)
import Data.Either (Either, hush)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), isJust, fromMaybe)
import Data.Bifunctor (bimap)
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Aff.Bus as Bus
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Effect.Ref as Ref
import Halogen as H
import Halogen.Aff as HA
import Halogen.Aff.Util as HU
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)
import Route as Route
import Routing.Duplex (parse, print)
import Routing.Hash (matchesWith, setHash)
import Web.DOM.ParentNode (QuerySelector(..))

main :: Effect Unit
main =
  HA.runHalogenAff do
    _ <- HA.awaitBody
    currentUser <- liftEffect $ Ref.new Nothing
    userBus <- Bus.make
    -- get current user
    liftEffect Api.readUser
      >>= traverse_ \user -> do
          res <- H.liftAff $ Api.login user
          let
            fetchedUser :: Either String _
            fetchedUser = bimap (_.error) (const user) res
          liftEffect $ Ref.write (hush fetchedUser) currentUser
    -- define component and environment
    let
      environment :: Env
      environment = { sessionEnv }
        where
        sessionEnv :: SessionEnv
        sessionEnv = { currentUser, userBus }

      rootComponent :: H.Component HH.HTML Router.Query {} Void Aff.Aff
      rootComponent = H.hoist (runAppM environment) Router.component
    mbEl <- HU.selectElement $ QuerySelector ".app"
    case mbEl of
      Just el -> do
        -- render
        halogenIO <- runUI rootComponent {} el
        -- listen for location change and notify user
        void $ liftEffect
          $ matchesWith (parse Route.routeCodec) \old new ->
              when (old /= Just new) do
                user <- liftEffect $ Ref.read environment.sessionEnv.currentUser
                -- if logged in and visits Login
                -- then go to old route (or Dashboard)
                -- otherwise navigate to next route
                case isJust user && new == Route.Login of
                  true -> liftEffect $ setHash $ print Route.routeCodec (fromMaybe Route.Dashboard old)
                  false -> Aff.launchAff_ $ halogenIO.query $ H.tell $ Router.Navigate new
      Nothing -> liftEffect $ throw "Could not mount app"
