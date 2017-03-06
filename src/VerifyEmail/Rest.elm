module VerifyEmail.Rest exposing (..)

import App.Config as Config
import Http exposing (..)
import Json.Decode as Decode exposing (field)
import Json.Encode as Encode exposing (Value, encode, object, string)
import VerifyEmail.Types exposing (..)


url : String
url =
    String.concat [ Config.baseUrl, "auth/registration/verify-email/" ]


verifyEmailDecoder : Decode.Decoder VerifyEmailModel
verifyEmailDecoder =
    Decode.map VerifyEmailModel
        (field "detail" Decode.string)


verifyEmailEncoder : Model -> Value
verifyEmailEncoder model =
    object
        [ ( "key", string model.key )
        ]


bodyBuild : Model -> Body
bodyBuild model =
    stringBody "application/json" (encode 0 (verifyEmailEncoder model))


postVerifyEmail : Model -> Http.Request VerifyEmailModel
postVerifyEmail model =
    Http.post url (bodyBuild model) verifyEmailDecoder


fetchVerifyEmail : Model -> Cmd Msg
fetchVerifyEmail model =
    Http.send OnFetchVerifyEmail (postVerifyEmail model)
