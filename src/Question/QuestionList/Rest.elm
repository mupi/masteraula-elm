module Question.QuestionList.Rest exposing (..)

import Http
import Json.Decode as Decode exposing (field)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)
import Json.Encode as Encode exposing (encode)


-- My modules

import App.Config as Config
import Question.QuestionList.Types exposing (..)
import Question.Question.Rest as Question
import User.Rest as User


-- Decoders


questionOrderDecoder : Decode.Decoder QuestionOrder
questionOrderDecoder =
    decode QuestionOrder
        |> required "question" Question.questionDecoder
        |> required "order" Decode.int


questionListDecoder : Decode.Decoder QuestionList
questionListDecoder =
    decode QuestionList
        |> required "id" Decode.int
        |> required "question_list_header" Decode.string
        |> required "secret" Decode.bool
        |> required "owner" User.userDecoder
        |> optional "questions" (Decode.list questionOrderDecoder) []
        |> required "question_count" Decode.int
        |> required "create_date" Decode.string



-- Encoders


questionListEncoder : QuestionList -> Encode.Value
questionListEncoder questionList =
    Encode.object
        [ ( "id", Encode.int questionList.id )
        , ( "question_list_header", Encode.string questionList.question_list_header )
        , ( "secret", Encode.bool questionList.secret )
        , ( "owner", User.userEncoder questionList.owner )
        , ( "questions", Encode.list <| List.map questionOrderEncoder questionList.questions )
        , ( "question_count", Encode.int questionList.question_count )
        , ( "create_date", Encode.string questionList.create_date )
        ]


questionOrderEncoder : QuestionOrder -> Encode.Value
questionOrderEncoder questionOrder =
    Encode.object
        [ ( "question", Question.questionEncoder questionOrder.question )
        , ( "order", Encode.int questionOrder.order )
        ]


questionOrderSaveEncoder : Int -> QuestionOrder -> Encode.Value
questionOrderSaveEncoder newOrder questionOrder =
    Encode.object
        [ ( "question", Encode.int questionOrder.question.id )
        , ( "order", Encode.int (newOrder + 1) )
        ]


saveQuestionListEncoder : QuestionList -> Encode.Value
saveQuestionListEncoder questionList =
    Encode.object
        [ ( "secret", Encode.bool False )
        , ( "questions", Encode.list (List.indexedMap questionOrderSaveEncoder questionList.questions) )
        , ( "question_list_header", Encode.string questionList.question_list_header )
        ]



-- Header


headerBuild : Maybe String -> List Http.Header
headerBuild token =
    case token of
        Just token ->
            [ Http.header "Authorization" (String.concat [ "JWT ", token ]) ]

        Nothing ->
            []



-- Question List


urlQuestionList : Int -> String
urlQuestionList questionListId =
    if questionListId <= 0 then
        String.concat [ Config.baseUrl, "question_lists/" ]
    else
        String.concat [ Config.baseUrl, "question_lists/", toString questionListId, "/" ]


saveQuestionListBody : QuestionList -> Http.Body
saveQuestionListBody questionList =
    Http.stringBody "application/json" (encode 0 (saveQuestionListEncoder questionList))


postSaveQuestionList : QuestionList -> Maybe String -> Http.Request Int
postSaveQuestionList questionList token =
    Http.request
        { method =
            if questionList.id <= 0 then
                "POST"
            else
                "PATCH"
        , headers = (headerBuild token)
        , url = (urlQuestionList questionList.id)
        , body = saveQuestionListBody questionList
        , expect = (Http.expectJson (field "id" Decode.int))
        , timeout = Nothing
        , withCredentials = False
        }


fetchPostSaveQuestionList : QuestionList -> Maybe String -> Cmd Msg
fetchPostSaveQuestionList questionList token =
    Http.send OnFetchSaveQuestionList (postSaveQuestionList questionList token)


deleteQuestionList : QuestionList -> Maybe String -> Http.Request String
deleteQuestionList questionList token =
    Http.request
        { method = "DELETE"
        , headers = (headerBuild token)
        , url = (urlQuestionList questionList.id)
        , body = Http.emptyBody
        , expect = (Http.expectString)
        , timeout = Nothing
        , withCredentials = False
        }


fetchDeleteQuestionList : QuestionList -> Maybe String -> Cmd Msg
fetchDeleteQuestionList question token =
    Http.send OnFetchDeleteQuestionList (deleteQuestionList question token)


getQuestionList : Int -> Maybe String -> Http.Request QuestionList
getQuestionList questionListId token =
    Http.request
        { method = "GET"
        , headers = (headerBuild token)
        , url = (urlQuestionList questionListId)
        , body = Http.emptyBody
        , expect = (Http.expectJson questionListDecoder)
        , timeout = Nothing
        , withCredentials = False
        }


fetchGetQuestionList : Int -> Maybe String -> Cmd Msg
fetchGetQuestionList questionListId token =
    Http.send OnFetchGetQuestionList (getQuestionList questionListId token)
