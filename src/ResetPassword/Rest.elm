module ResetPassword.Rest exposing (..)

import App.Config as Config
import Http exposing (..)
import Json.Decode as Decode exposing (field)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)
import Json.Encode as Encode exposing (Value, encode, object, string)
import ResetPassword.Types exposing (..)


url : String
url =
    String.concat [ Config.baseUrl, "auth/password/reset/confirm/" ]


resetPassowrdEncoder : Model -> Value
resetPassowrdEncoder model =
    object
        [ ( "uid", string model.codUser )
        , ( "token", string model.key )
        , ( "new_password1", string model.newPassword )
        , ( "new_password2", string model.confirmPassword )
        ]


bodyBuild : Model -> Body
bodyBuild model =
    stringBody "application/json" (encode 0 (resetPassowrdEncoder model))


postResetPassword : Model -> Http.Request String
postResetPassword model =
    Http.request
        { method = "POST"
        , headers = []
        , url = url
        , body = (bodyBuild model)
        , expect = (Http.expectString)
        , timeout = Nothing
        , withCredentials = False
        }


fetchResetPassword : Model -> Cmd Msg
fetchResetPassword model =
    Http.send OnFetchResetPassword (postResetPassword model)
