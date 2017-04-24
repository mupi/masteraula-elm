module ResetPassword.Rest exposing (..)

import App.Config as Config
import Http exposing (..)
import Json.Encode as Encode exposing (Value, encode, object, string)
import ResetPassword.Types exposing (..)


urlSendEmail : String
urlSendEmail =
    String.concat [ Config.baseUrl, "auth/password/reset/" ]


sendEmailEncoder : Model -> Value
sendEmailEncoder model =
    object
        [ ( "email", string model.email )
        ]


bodyBuildSendEmail : Model -> Body
bodyBuildSendEmail model =
    stringBody "application/json" (encode 0 (sendEmailEncoder model))


postSendEmail : Model -> Http.Request String
postSendEmail model =
    Http.request
        { method = "POST"
        , headers = []
        , url = urlSendEmail
        , body = (bodyBuildSendEmail model)
        , expect = (Http.expectString)
        , timeout = Nothing
        , withCredentials = False
        }


fetchSendEmail : Model -> Cmd Msg
fetchSendEmail model =
    Http.send OnFetchSendEmail (postSendEmail model)



--


url : String
url =
    String.concat [ Config.baseUrl, "auth/password/reset/confirm/" ]


resetPasswordEncoder : Model -> Value
resetPasswordEncoder model =
    object
        [ ( "uid", string model.codUser )
        , ( "token", string model.key )
        , ( "new_password1", string model.newPassword )
        , ( "new_password2", string model.confirmPassword )
        ]


bodyBuild : Model -> Body
bodyBuild model =
    stringBody "application/json" (encode 0 (resetPasswordEncoder model))


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
