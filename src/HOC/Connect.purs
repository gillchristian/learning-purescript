module HOC.Connect where

import Prelude
import Data.Maybe (Maybe(..))
import Control.Monad.Reader.Trans (class MonadAsk, asks)
import Effect.Aff.Class (class MonadAff)
import Effect.Ref as Ref
import Halogen as H
import Halogen.HTML as HH
import Api as Api
import Prim.Row as Row
import Data.Symbol (SProxy(..))
import AppM (SessionEnv)
import Record as Record
import Util (busEventSource)

data Action input output
  = Initialize
  | HandleUserBus (Maybe Api.User)
  | Receive input
  | Emit output

type WithCurrentUser r
  = ( currentUser :: Maybe Api.User | r )

type ChildSlots query output
  = ( inner :: H.Slot query output Unit )

_inner = SProxy :: SProxy "inner"

component ::
  forall query input output m r.
  MonadAff m =>
  MonadAff m =>
  MonadAsk { sessionEnv :: SessionEnv | r } m =>
  Row.Lacks "currentUser" input =>
  H.Component HH.HTML query { | WithCurrentUser input } output m ->
  H.Component HH.HTML query { | input } output m
component innerComponent =
  H.mkComponent
    { initialState: Record.insert (SProxy :: _ "currentUser") Nothing
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              , handleQuery = handleQuery
              , initialize = Just Initialize
              , receive = Just <<< Receive
              }
    }
  where
  handleAction = case _ of
    Initialize -> do
      { currentUser, userBus } <- asks _.sessionEnv
      _ <- H.subscribe (HandleUserBus <$> busEventSource userBus)
      mbUser <- H.liftEffect $ Ref.read currentUser
      H.modify_ _ { currentUser = mbUser }
    HandleUserBus mbUser ->
       H.modify_ _ { currentUser = mbUser }
    Receive input -> do
      { currentUser } <- H.get
      H.put $ Record.insert (SProxy :: _ "currentUser") currentUser input
    Emit output -> H.raise output

  handleQuery :: forall a. query a -> H.HalogenM _ _ _ _ _ (Maybe a)
  handleQuery = H.query _inner unit

  render state = HH.slot _inner unit innerComponent state (Just <<< Emit)
