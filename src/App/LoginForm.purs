module App.LoginForm where

import Prelude

import DOM.HTML.Indexed.InputType (InputType(..))
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (delay)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.Event.Event (Event)
import Web.Event.Event as Event

data Status err succ = 
    Wip
  | Loading
  | Failure err
  | Success succ

type State =
  { email :: String
  , password :: String
  , status :: Status String Unit
  }

data Action =
    PasswordChange String
  | EmailChange String
  | OnLogin Event


component :: forall q i o m. MonadAff m => H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState: \_ -> { email: "", password: "", status: Wip }
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

render :: forall cs m. State -> H.ComponentHTML Action cs m
render state =
  HH.form
    [ HE.onSubmit (Just <<< OnLogin) ]
    [ HH.p_ [ HH.text $ "Sign in" ]
    , HH.div_
      [ HH.label_ [ HH.text "Email" ]
      , HH.input
          [ HE.onValueInput (Just <<< EmailChange)
          , HP.value state.email
          , HP.disabled $ shouldDisable state.status
          ]
      ]
    , HH.div_
      [ HH.label_ [ HH.text "Password" ]
      , HH.input
          [ HE.onValueInput (Just <<< PasswordChange)
          , HP.type_ InputPassword
          , HP.value state.password
          , HP.disabled $ shouldDisable state.status
          ]
      ]
    , HH.div_
      [ HH.button
          [ HP.type_ HP.ButtonSubmit, HP.disabled $ shouldDisable state.status ]
          [ HH.text "Login" ]
      ]
    , HH.div_
      [ case state.status of
          Failure msg -> HH.text msg
          _ -> HH.text ""
      ]
    ]

handleAction :: forall cs o m. MonadAff m => Action → H.HalogenM State Action cs o m Unit
handleAction = case _ of
  EmailChange newEmail ->
    H.modify_ _ { email = newEmail }

  PasswordChange newPassword ->
    H.modify_ _ { password = newPassword }

  OnLogin event -> do
    H.liftEffect $ Event.preventDefault event
    H.modify_ _ { status = Loading }
    H.liftAff $ delay $ Milliseconds 1000.0 -- TODO: make a request
    H.modify_ _ { status = Failure "Could not login" }

shouldDisable :: forall err succ. Status err succ -> Boolean
shouldDisable Loading = true
shouldDisable (Success _) = true
shouldDisable _ = false
