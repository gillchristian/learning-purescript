module AppM where

import Prelude
import Control.Monad.Reader.Trans (class MonadAsk, ReaderT, runReaderT, asks)
import Data.Maybe (Maybe(..))
import Effect.Aff.Bus as Bus
import Effect.Ref (Ref)
import Effect.Aff (Aff)
import Effect.Ref as Ref
import Api as Api
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Type.Equality (class TypeEquals, from)
import Routing.Hash (setHash)
import Routing.Duplex (print)
import Navigate (class Navigate, navigate)
import Route as Route

type SessionEnv
  = { currentUser :: Ref (Maybe Api.User)
    , userBus :: Bus.BusRW (Maybe Api.User)
    }

type Env
  = { sessionEnv :: SessionEnv
    }

newtype AppM a
  = AppM (ReaderT Env Aff a)

runAppM :: Env -> AppM ~> Aff
runAppM env (AppM m) = runReaderT m env

derive newtype instance functorAppM :: Functor AppM

derive newtype instance applyAppM :: Apply AppM

derive newtype instance applicativeAppM :: Applicative AppM

derive newtype instance bindAppM :: Bind AppM

derive newtype instance monadAppM :: Monad AppM

derive newtype instance monadEffectAppM :: MonadEffect AppM

derive newtype instance monadAffAppM :: MonadAff AppM

instance monadAskAppM :: TypeEquals e Env => MonadAsk e AppM where
  ask = AppM $ asks from

instance navigateAppM :: Navigate AppM where
  navigate = liftEffect <<< setHash <<< print Route.routeCodec
  logout = do
    { currentUser, userBus } <- asks _.sessionEnv
    liftEffect do
      Ref.write Nothing currentUser
      Api.removeUser
    liftAff do
      Bus.write Nothing userBus
    navigate Route.Home
