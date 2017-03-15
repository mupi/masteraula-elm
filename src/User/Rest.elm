module User.Rest exposing (..)

import App.Config as Config
import Http exposing (..)
import Json.Decode as Decode exposing (field)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)
import Json.Encode as Encode exposing (Value, encode, object, string)
import User.Types exposing (..)
import Json.Decode as Decode exposing (field)
import User.Types exposing (User)


usersDecoder : Decode.Decoder (List User)
usersDecoder =
    Decode.list userDecoder


userDecoder : Decode.Decoder User
userDecoder =
    decode User
        |> required "id" Decode.int
        |> required "username" Decode.string
        |> required "name" Decode.string
        |> optional "email" Decode.string ""


headerBuild : Maybe String -> List Http.Header
headerBuild token =
    case token of
        Just token ->
            [ Http.header "Authorization" (String.concat [ "JWT ", token ]) ]

        Nothing ->
            []


bodyBuild : Model -> (Model -> Value) -> Body
bodyBuild model encoder =
    stringBody "application/json" (encode 0 (encoder model))



-- Change Password


changePasswordUrl : String
changePasswordUrl =
    String.concat [ Config.baseUrl, "auth/password/change/" ]


changePasswordEncoder : Model -> Value
changePasswordEncoder model =
    object
        [ ( "old_password", string model.password )
        , ( "new_password1", string model.newPassword )
        , ( "new_password2", string model.newPassword )
        ]


postChangePassword : Model -> Maybe String -> Http.Request String
postChangePassword model token =
    Http.request
        { method = "POST"
        , headers = headerBuild token
        , url = changePasswordUrl
        , body = (bodyBuild model changePasswordEncoder)
        , expect = (Http.expectString)
        , timeout = Nothing
        , withCredentials = False
        }


fetchChangePassword : Model -> Maybe String -> Cmd Msg
fetchChangePassword model token =
    Http.send OnFetchPasswordChange (postChangePassword model token)



-- Update Profile


profileUpdateUrl : String
profileUpdateUrl =
    String.concat [ Config.baseUrl, "auth/user/" ]


profileUpdateEncoder : Model -> Value
profileUpdateEncoder model =
    let
        editUser =
            model.editUser

        user =
            model.user
    in
        if user.email /= editUser.email then
            object
                [ ( "name", string editUser.name )
                , ( "email", string editUser.email )
                ]
        else
            object
                [ ( "name", string editUser.name ) ]


patchProfileUpdate : Model -> Maybe String -> Http.Request String
patchProfileUpdate model token =
    Http.request
        { method = "PATCH"
        , headers = headerBuild token
        , url = profileUpdateUrl
        , body = (bodyBuild model profileUpdateEncoder)
        , expect = (Http.expectString)
        , timeout = Nothing
        , withCredentials = False
        }


fetchProfileUpdate : Model -> Maybe String -> Cmd Msg
fetchProfileUpdate model token =
    Http.send OnFetchProfileUpdate (patchProfileUpdate model token)



-- Get User


urlUser : UserId -> String
urlUser userId =
    String.concat [ Config.baseUrl, "users/", (toString userId), "/" ]


getUser : UserId -> Maybe String -> Http.Request User
getUser userId token =
    Http.request
        { method = "GET"
        , headers = (headerBuild token)
        , url = (urlUser userId)
        , body = Http.emptyBody
        , expect = (Http.expectJson userDecoder)
        , timeout = Nothing
        , withCredentials = False
        }


fetchGetUser : UserId -> Maybe String -> Cmd Msg
fetchGetUser userId token =
    Http.send OnFetchGetUser (getUser userId token)
