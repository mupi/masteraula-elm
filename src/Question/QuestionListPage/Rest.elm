module Question.QuestionListPage.Rest exposing (..)

import Http
import Json.Decode as Decode exposing (field)


-- My modules

import App.Config as Config
import Question.QuestionListPage.Types exposing (..)
import Question.QuestionList.Rest as QuestionList
import Question.QuestionList.Types as QuestionList


-- Decoders
-- questionListPageDecoder : Int -> Decode.Decoder QuestionListPage
-- questionListPageDecoder page =
--     decode QuestionListPage
--         |> required "count" Decode.int
--         |> hardcoded page
--         |> required "next" (Decode.nullable Decode.string)
--         |> required "previous" (Decode.nullable Decode.string)
--         |> required "results" (Decode.list QuestionList.questionListDecoder)


headerBuild : Maybe String -> List Http.Header
headerBuild token =
    case token of
        Just token ->
            [ Http.header "Authorization" (String.concat [ "JWT ", token ]) ]

        Nothing ->
            []



-- Question List Pages


urlListMineQuestionList : String
urlListMineQuestionList =
    String.concat [ Config.baseUrl, "question_lists/user_list_questions/" ]


getMineQuestionList : Maybe String -> Http.Request (List QuestionList.QuestionList)
getMineQuestionList token =
    Http.request
        { method = "GET"
        , headers = (headerBuild token)
        , url = urlListMineQuestionList
        , body = Http.emptyBody
        , expect = (Http.expectJson <| Decode.list QuestionList.questionListDecoder)
        , timeout = Nothing
        , withCredentials = False
        }


fetchGetMineQuestionList : Maybe String -> Cmd Msg
fetchGetMineQuestionList token =
    Http.send OnFetchGetMineQuestionListPage (getMineQuestionList token)
