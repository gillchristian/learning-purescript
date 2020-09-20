module App.LoginForm where

import Prelude
import AppM (SessionEnv)
import Api (login, writeUser)
import DOM.HTML.Indexed.InputType (InputType(..))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Control.Monad.Reader.Trans (class MonadAsk, ask)
import Effect.Aff.Class (class MonadAff)
import Effect.Ref as Ref
import Effect.Aff.Bus as Bus
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Navigate (class Navigate, navigate)
import Route as Route
import Util (classes)
import Web.Event.Event (Event)
import Web.Event.Event as Event

-- TODO: { redirect :: Bool } on input
data Status err succ
  = Wip
  | Loading
  | Failure err
  | Success succ

type State
  = { email :: String
    , password :: String
    , status :: Status String Unit
    }

data Action
  = PasswordChange String
  | EmailChange String
  | OnLogin Event

component ::
  forall q i o m r.
  MonadAff m =>
  Navigate m =>
  MonadAsk { sessionEnv :: SessionEnv | r } m =>
  H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState: \_ -> { email: "", password: "", status: Wip }
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
  handleAction :: forall cs. Action → H.HalogenM State Action cs o m Unit
  handleAction = case _ of
    EmailChange newEmail -> H.modify_ _ { email = newEmail }
    PasswordChange newPassword -> H.modify_ _ { password = newPassword }
    OnLogin event -> do
      H.liftEffect $ Event.preventDefault event
      H.modify_ _ { status = Loading }
      state <- H.get
      let
        user = { email: state.email, password: state.password }
      response <- H.liftAff $ login user
      case response of
        Left { error } -> H.modify_ _ { status = Failure error }
        Right _ -> do
          -- TODO: Bus and Ref to util
          { sessionEnv } <- ask
          H.liftEffect do
            Ref.write (Just user) sessionEnv.currentUser
            writeUser user
          H.liftAff $ Bus.write (Just user) sessionEnv.userBus
          H.modify_ _ { status = Success unit }
          -- TODO stay on current route if different from Login ?
          navigate Route.Dashboard

render :: forall cs m. State -> H.ComponentHTML Action cs m
render state =
  HH.div [ classes [ "w-full", "max-w-xs" ] ]
    [ HH.form
        [ HE.onSubmit (Just <<< OnLogin), classes [ "bg-white", "shadow-md", "rounded", "px-8", "pt-6", "pb-8", "mb-4" ] ]
        [ HH.div [ classes [ "mb-4" ] ]
            [ HH.label [ classes [ "block", "text-gray-700", "text-sm", "font-bold", "mb-2" ] ]
                [ HH.text "Email" ]
            , HH.input
                [ HE.onValueInput (Just <<< EmailChange)
                , HP.value state.email
                , HP.type_ InputEmail
                , HP.disabled $ isDisabled
                , classes
                    [ "shadow"
                    , "appearance-none border"
                    , "rounded"
                    , "w-full"
                    , "py-2"
                    , "px-3"
                    , "text-gray-700"
                    , "leading-tight"
                    , "focus:outline-none"
                    , "focus:shadow-outline"
                    ]
                ]
            ]
        , HH.div [ classes [ "mb-6" ] ]
            [ HH.label [ classes [ "block", "text-gray-700", "text-sm", "font-bold", "mb-2" ] ]
                [ HH.text "Password" ]
            , HH.input
                [ HE.onValueInput (Just <<< PasswordChange)
                , HP.type_ InputPassword
                , HP.placeholder "******************"
                , HP.value state.password
                , HP.disabled $ isDisabled
                , classes
                    [ "mb-3"
                    , "shadow"
                    , "appearance-none"
                    , "border"
                    -- TODO: only on error
                    , "border-red-500"
                    , "rounded"
                    , "w-full"
                    , "py-2"
                    , "px-3"
                    , "text-gray-700"
                    , "leading-tight"
                    , "focus:outline-none"
                    , "focus:shadow-outline"
                    ]
                ]
            ]
        , HH.div [ classes [ "flex", "items-center", "justify-between" ] ]
            [ HH.button
                [ HP.type_ HP.ButtonSubmit
                , HP.disabled $ isDisabled
                , classes
                    [ "bg-blue-500"
                    , "hover:bg-blue-700"
                    , "text-white"
                    , "font-bold"
                    , "py-2"
                    , "px-4"
                    , "rounded"
                    , "focus:outline-none"
                    , "focus:shadow-outline"
                    , if isDisabled then "opacity-50" else ""
                    , if isDisabled then "cursor-not-allowed" else ""
                    ]
                ]
                [ HH.text "Login" ]
            ]
        , HH.div_
            [ case state.status of
                Failure msg ->
                  HH.div
                    [ classes
                        [ "mt-4"
                        , "bg-red-100"
                        , "border"
                        , "border-red-400"
                        , "text-red-700"
                        , "px-4"
                        , "py-3"
                        , "rounded"
                        , "relative"
                        ]
                    ]
                    [ HH.span [ classes [ "block", "sm:inline" ] ] [ HH.text msg ] ]
                Success _ -> HH.text "Success"
                Loading -> HH.text "Loading"
                Wip -> HH.text "Wip"
            ]
        ]
    ]
  where
  isDisabled = shouldDisable state.status

shouldDisable :: forall err succ. Status err succ -> Boolean
shouldDisable = case _ of
  Loading -> true
  (Success _) -> true
  _ -> false

eitherToStatus :: forall err succ. Either err succ -> Status err succ
eitherToStatus = case _ of
  (Right succ) -> Success succ
  (Left err) -> Failure err
