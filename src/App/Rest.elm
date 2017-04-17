module App.Rest exposing (..)

import Json.Decode as Decode exposing (..)
import App.Types exposing (..)
import Question.Rest as Question exposing (..)
import User.Rest as User exposing (..)


localStorageDecoder : Decode.Decoder LocalStorage
localStorageDecoder =
    Decode.map3 LocalStorage
        (field "user" (nullable User.userDecoder))
        (field "token" (nullable string))
        (field "questionList" Question.questionListDecoder)
