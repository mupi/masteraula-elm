module User.Rest exposing (..)

import Http
import Json.Decode as Decode exposing (field)
import User.Types exposing (User)


usersDecoder : Decode.Decoder (List User)
usersDecoder =
    Decode.list userDecoder


userDecoder : Decode.Decoder User
userDecoder =
    Decode.map4 User
        (field "id" Decode.int)
        (field "username" Decode.string)
        (field "name" Decode.string)
        (field "email" Decode.string)
