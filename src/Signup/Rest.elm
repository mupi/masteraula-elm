module Signup.Rest exposing (..)

import Http exposing (..)
import Json.Decode as Decode exposing (field)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)
import Json.Encode as Encode exposing (Value, encode, object, string)
import Signup.Types exposing (..)


url : String
url =
    "http://localhost:8000/rest/auth/registration/"


signupDecoder : Decode.Decoder RestErrorModel
signupDecoder =
    decode RestErrorModel
        |> optional "email" (Decode.list Decode.string) []
        |> optional "username" (Decode.list Decode.string) []
        |> optional "password1" (Decode.list Decode.string) []
        |> optional "password2" (Decode.list Decode.string) []
        |> optional "non_field_errors" (Decode.list Decode.string) []


signupEncoder : Model -> Value
signupEncoder model =
    object
        [ ( "username", string model.username )
        , ( "password1", string model.password )
        , ( "password2", string model.confirmPassword )
        , ( "email", string model.email )
        ]


bodyBuild : Model -> Body
bodyBuild model =
    stringBody "application/json" (encode 0 (signupEncoder model))


postLogin : Model -> Http.Request RestErrorModel
postLogin model =
    Http.post url (bodyBuild model) signupDecoder


fetchSignUp : Model -> Cmd Msg
fetchSignUp model =
    Http.send OnFetchSignup (postLogin model)


decodeErrors : String -> RestErrorModel
decodeErrors jsonValue =
    let
        res =
            Decode.decodeString signupDecoder jsonValue
    in
        case res of
            Ok value ->
                value

            Err error ->
                RestErrorModel [ (toString error) ] [] [] [] []
