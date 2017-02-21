module Question.Rest exposing (..)

import Http
import Json.Decode as Decode exposing (field)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)
import Json.Encode as Encode exposing (Value, encode, object, string, list, int, bool)
import Question.Types exposing (..)
import Utils.StringUtils exposing (..)


-- Decoders


answerDecoder : Decode.Decoder Answer
answerDecoder =
    Decode.map3 Answer
        (field "id" Decode.int)
        (field "answer_text" Decode.string)
        (field "is_correct" Decode.bool)


questionDecoder : Decode.Decoder Question
questionDecoder =
    decode Question
        |> required "id" Decode.int
        |> required "question_header" Decode.string
        |> required "question_text" Decode.string
        |> required "level" (Decode.nullable Decode.string)
        |> required "author" Decode.string
        |> required "credit_cost" Decode.int
        |> required "url" Decode.string
        |> required "tags" (Decode.list Decode.string)
        |> optional "answers" (Decode.list answerDecoder) []


questionPageDecoder : PageNumber -> Decode.Decoder QuestionPage
questionPageDecoder page =
    decode QuestionPage
        |> required "count" Decode.int
        |> hardcoded page
        |> required "next" (Decode.nullable Decode.string)
        |> required "previous" (Decode.nullable Decode.string)
        |> required "results" (Decode.list questionDecoder)


headerBuild : Maybe String -> List Http.Header
headerBuild token =
    case token of
        Just token ->
            [ Http.header "Authorization" (String.concat [ "JWT ", token ]) ]

        Nothing ->
            []



-- Question


urlQuestion : QuestionId -> String
urlQuestion questionId =
    String.concat [ "http://localhost:8000/rest/questions/", (toString questionId), "/" ]


getQuestion : QuestionId -> Maybe String -> Http.Request Question
getQuestion questionId token =
    Http.request
        { method = "GET"
        , headers = (headerBuild token)
        , url = (urlQuestion questionId)
        , body = Http.emptyBody
        , expect = (Http.expectJson questionDecoder)
        , timeout = Nothing
        , withCredentials = False
        }


fetchGetQuestion : QuestionId -> Maybe String -> Cmd Msg
fetchGetQuestion questionId token =
    Http.send OnFetchGetQuestion (getQuestion questionId token)



--Question Page


urlQuestionPage : PageNumber -> String
urlQuestionPage page =
    String.concat [ "http://localhost:8000/rest/questions/?page=", (toString page) ]


getQuestionPage : PageNumber -> Maybe String -> Http.Request QuestionPage
getQuestionPage page token =
    Http.request
        { method = "GET"
        , headers = (headerBuild token)
        , url = (urlQuestionPage page)
        , body = Http.emptyBody
        , expect = (Http.expectJson <| questionPageDecoder page)
        , timeout = Nothing
        , withCredentials = False
        }


fetchGetQuestionPage : PageNumber -> Maybe String -> Cmd Msg
fetchGetQuestionPage page token =
    Http.send OnFetchGetQuestionPage (getQuestionPage page token)



-- Question Search


urlBaseSearch : PageNumber -> String
urlBaseSearch page =
    String.concat [ "http://localhost:8000/rest/search/question/?page=", toString page ]


urlTagSearchQuestion : String -> List String -> String
urlTagSearchQuestion baseUrl tags =
    String.concat
        (baseUrl
            :: "&tags="
            :: List.map
                (\t -> String.concat [ tagFormatter t, "," ])
                tags
        )


urlSearch : PageNumber -> List String -> String
urlSearch page tags =
    if List.length tags > 0 then
        urlTagSearchQuestion (urlBaseSearch page) tags
    else
        urlBaseSearch page


getQuestionTagSearch : PageNumber -> List String -> Maybe String -> Http.Request QuestionPage
getQuestionTagSearch page tags token =
    Http.request
        { method = "GET"
        , headers = (headerBuild token)
        , url = (urlSearch page tags)
        , body = Http.emptyBody
        , expect = (Http.expectJson <| questionPageDecoder page)
        , timeout = Nothing
        , withCredentials = False
        }


fetchGetQuestionTagSearch : PageNumber -> List String -> Maybe String -> Cmd Msg
fetchGetQuestionTagSearch page tags token =
    Http.send OnFetchGetQuestionTagSearch (getQuestionTagSearch page tags token)



-- Question List


urlSaveList : String
urlSaveList =
    "http://localhost:8000/rest/question_lists/"


questionEncoder : Int -> Question -> Value
questionEncoder index question =
    object
        [ ( "question", string (String.concat [ "http://localhost:8000/rest/questions/", toString question.id, "/" ]) )
        , ( "order", int (index + 1) )
        ]


saveQuestionListEncoder : QuestionList -> Value
saveQuestionListEncoder questionList =
    object
        [ ( "private", bool False )
        , ( "questions", list (List.indexedMap questionEncoder questionList.questions) )
        , ( "question_list_header", string questionList.question_list_header )
        ]


saveQuestionListBody : QuestionList -> Http.Body
saveQuestionListBody questionList =
    Http.stringBody "application/json" (encode 0 (saveQuestionListEncoder questionList))


postSaveQuestioList : QuestionList -> Maybe String -> Http.Request String
postSaveQuestioList questionList token =
    let
        a =
            Debug.log
                "a"
                (Http.request
                    { method = "POST"
                    , headers = (headerBuild token)
                    , url = urlSaveList
                    , body = saveQuestionListBody questionList
                    , expect = (Http.expectJson (field "url" Decode.string))
                    , timeout = Nothing
                    , withCredentials = False
                    }
                )
    in
        Http.request
            { method = "POST"
            , headers = (headerBuild token)
            , url = urlSaveList
            , body = saveQuestionListBody questionList
            , expect = (Http.expectJson (field "url" Decode.string))
            , timeout = Nothing
            , withCredentials = False
            }


fetchPostSaveQuestioList : QuestionList -> Maybe String -> Cmd Msg
fetchPostSaveQuestioList question token =
    Http.send OnFetchSaveQuestionList (postSaveQuestioList question token)



-- Question List (Docx file generation)


urlGenerateList : Int -> String
urlGenerateList id =
    String.concat [ "http://localhost:8000/rest/question_lists/", toString id, "/generate_list/" ]


getGenerateList : Int -> Maybe String -> Http.Request String
getGenerateList id token =
    Http.request
        { method = "GET"
        , headers = (headerBuild token)
        , url = (urlGenerateList id)
        , body = Http.emptyBody
        , expect = (Http.expectJson (field "code" Decode.string))
        , timeout = Nothing
        , withCredentials = False
        }


fetchGetGenerateList : Int -> Maybe String -> Cmd Msg
fetchGetGenerateList id token =
    Http.send OnFecthQuestionListGenerate (getGenerateList id token)
