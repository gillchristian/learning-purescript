module Api where

import Prelude
import Affjax as AX
import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as ResponseFormat
import Affjax.StatusCode (StatusCode(..))
import Data.Argonaut (decodeJson, encodeJson, stringify) as Json
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Parser (jsonParser) as Json
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either, hush)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff as Aff
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem, removeItem, setItem)

newtype Token
  = Token String

derive instance eqToken :: Eq Token

derive instance ordToken :: Ord Token

instance showToken :: Show Token where
  show (Token _) = "Token {- token -}"

type User
  = { email :: String
    , password :: String
    }

loginUrl :: String
loginUrl = "https://reqres.in/api/login"

login :: User -> Aff.Aff (Either { error :: String } { token :: String })
login credentials = do
  result <-
    AX.post ResponseFormat.json loginUrl
      $ Just
      $ RequestBody.json
      $ Json.encodeJson credentials
  case result of
    Left _ -> pure $ Left { error: "Failed to login" }
    Right response
      | response.status < (StatusCode 300) ->
        pure
          $ lmap { error: _ }
          $ Json.decodeJson response.body
    Right response ->
      pure
        $ either (Left <<< { error: _ }) (Left <<< identity)
        $ Json.decodeJson response.body

parseJson :: forall a. DecodeJson a => String -> Maybe a
parseJson = hush <<< (Json.decodeJson <=< Json.jsonParser)

encodeJson :: forall a. EncodeJson a => a -> String
encodeJson = Json.stringify <<< Json.encodeJson

userKey :: String
userKey = "SESSION_USER"

readUser :: Effect (Maybe User)
readUser = (parseJson =<< _) <$> (getItem userKey =<< localStorage =<< window)

writeUser :: User -> Effect Unit
writeUser user = setItem userKey (encodeJson user) =<< localStorage =<< window

removeUser :: Effect Unit
removeUser = removeItem userKey =<< localStorage =<< window
