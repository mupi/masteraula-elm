module Question.Question.Rest exposing (..)

import Http
import Json.Decode as Decode exposing (field)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)
import Json.Encode as Encode exposing (encode)


-- My modules

import App.Config as Config
import User.Rest as User
import Question.Question.Types exposing (..)
import Utils.EncodeUtils as Encode


-- Decoders


subjectDecoder : Decode.Decoder Subject
subjectDecoder =
    Decode.map3
        Subject
        (field "id" Decode.int)
        (field "subject_name" Decode.string)
        (field "slug" Decode.string)


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
        |> optional "question_parent" (Decode.map QuestionParent (Decode.nullable (Decode.lazy (\_ -> questionDecoder)))) (QuestionParent Nothing)
        |> required "question_statement" Decode.string
        |> required "level" (Decode.nullable Decode.string)
        |> required "author" User.userDecoder
        |> required "credit_cost" Decode.int
        |> required "tags" (Decode.list Decode.string)
        |> optional "resolution" (Decode.nullable Decode.string) Nothing
        |> optional "answers" (Decode.list answerDecoder) []
        |> optional "subjects" (Decode.list subjectDecoder) []
        |> required "education_level" (Decode.nullable Decode.string)
        |> required "year" (Decode.nullable Decode.int)
        |> required "source" (Decode.nullable Decode.string)
        |> optional "question_lists" (Decode.list questionListInfoDecoder) []
        |> optional "related_questions" (Decode.map RelatedQuestion (Decode.list (Decode.lazy (\_ -> questionDecoder)))) (RelatedQuestion [])


questionListInfoDecoder : Decode.Decoder QuestionListInfo
questionListInfoDecoder =
    decode QuestionListInfo
        |> required "id" Decode.int
        |> required "question_list_header" Decode.string
        |> required "secret" Decode.bool
        |> required "owner" User.userDecoder
        |> required "question_count" Decode.int
        |> required "create_date" Decode.string



-- Encoders


questionEncoder : Question -> Encode.Value
questionEncoder question =
    Encode.object
        [ ( "id", Encode.int question.id )
        , case question.question_parent of
            QuestionParent (Just parent) ->
                ( "question_parent", questionEncoder parent )

            _ ->
                ( "question_parent", Encode.null )
        , ( "question_statement", Encode.string question.question_statement )
        , ( "level", Encode.maybeString question.level )
        , ( "author", User.userEncoder question.author )
        , ( "credit_cost", Encode.int question.credit_cost )
        , ( "tags", Encode.list <| List.map Encode.string question.tags )
        , ( "resolution", Encode.maybeString question.resolution )
        , ( "answers", Encode.list <| List.map answerEncoder question.answers )
        , ( "subjects", Encode.list <| List.map subjectEncoder question.subjects )
        , ( "education_level", Encode.maybeString question.education_level )
        , ( "year", Encode.maybeInt question.year )
        , ( "source", Encode.maybeString question.source )
        , ( "question_lists", Encode.list <| List.map questionListInfoEncoder question.question_lists )
        , case question.related_questions of
            RelatedQuestion [] ->
                ( "related_questions", Encode.list [] )

            RelatedQuestion qc ->
                ( "related_questions", Encode.list <| List.map questionEncoder qc )
        ]


answerEncoder : Answer -> Encode.Value
answerEncoder answer =
    Encode.object
        [ ( "id", Encode.int answer.id )
        , ( "answer_text", Encode.string answer.answer_text )
        , ( "is_correct", Encode.bool answer.is_correct )
        ]


subjectEncoder : Subject -> Encode.Value
subjectEncoder subject =
    Encode.object
        [ ( "id", Encode.int subject.id )
        , ( "subject_name", Encode.string subject.subject_name )
        , ( "slug", Encode.string subject.slug )
        ]


questionListInfoEncoder : QuestionListInfo -> Encode.Value
questionListInfoEncoder questionListInfo =
    Encode.object
        [ ( "id", Encode.int questionListInfo.id )
        , ( "question_list_header", Encode.string questionListInfo.question_list_header )
        , ( "secret", Encode.bool questionListInfo.secret )
        , ( "owner", User.userEncoder questionListInfo.owner )
        , ( "question_count", Encode.int questionListInfo.question_count )
        , ( "create_date", Encode.string questionListInfo.create_date )
        ]



-- Header


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
    String.concat [ Config.baseUrl, "questions/", (toString questionId), "/" ]


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
