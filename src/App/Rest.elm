module App.Rest exposing (..)

import Json.Decode as Decode exposing (field)
import Json.Encode as Encode exposing (encode)


-- My modules

import App.Types exposing (..)
import Question.QuestionList.Rest as QuestionList exposing (..)
import User.Rest as User exposing (..)
import Utils.EncodeUtils as Encode


localStorageEncoder : LocalStorage -> Encode.Value
localStorageEncoder local =
    Encode.object
        [ case local.user of
            Just u ->
                ( "user", User.userEncoder u )

            Nothing ->
                ( "user", Encode.null )
        , ( "token", Encode.maybeString local.token )
        , ( "questionList", QuestionList.questionListEncoder local.questionList )
        ]


localStorageDecoder : Decode.Decoder LocalStorage
localStorageDecoder =
    Decode.map3 LocalStorage
        (field "user" (Decode.nullable User.userDecoder))
        (field "token" (Decode.nullable Decode.string))
        (field "questionList" QuestionList.questionListDecoder)
