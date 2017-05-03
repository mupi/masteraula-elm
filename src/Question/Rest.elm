module Question.Rest exposing (..)

import App.Config as Config
import Http
import Json.Decode as Decode exposing (field)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)
import Json.Encode as Encode exposing (encode)
import Question.Types exposing (..)
import User.Rest as User
import Utils.StringUtils exposing (..)
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


questionOrderDecoder : Decode.Decoder QuestionOrder
questionOrderDecoder =
    decode QuestionOrder
        |> required "question" questionDecoder
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


questionListInfoDecoder : Decode.Decoder QuestionListInfo
questionListInfoDecoder =
    decode QuestionListInfo
        |> required "id" Decode.int
        |> required "question_list_header" Decode.string
        |> required "secret" Decode.bool
        |> required "owner" User.userDecoder
        |> required "question_count" Decode.int
        |> required "create_date" Decode.string


questionPageDecoder : PageNumber -> Decode.Decoder QuestionPage
questionPageDecoder page =
    decode QuestionPage
        |> required "count" Decode.int
        |> hardcoded page
        |> required "next" (Decode.nullable Decode.string)
        |> required "previous" (Decode.nullable Decode.string)
        |> required "results" (Decode.list questionDecoder)


questionListPageDecoder : PageNumber -> Decode.Decoder QuestionListPage
questionListPageDecoder page =
    decode QuestionListPage
        |> required "count" Decode.int
        |> hardcoded page
        |> required "next" (Decode.nullable Decode.string)
        |> required "previous" (Decode.nullable Decode.string)
        |> required "results" (Decode.list questionListDecoder)


headerBuild : Maybe String -> List Http.Header
headerBuild token =
    case token of
        Just token ->
            [ Http.header "Authorization" (String.concat [ "JWT ", token ]) ]

        Nothing ->
            []



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


questionOrderEncoder : QuestionOrder -> Encode.Value
questionOrderEncoder questionOrder =
    Encode.object
        [ ( "question", questionEncoder questionOrder.question )
        , ( "order", Encode.int questionOrder.order )
        ]



-- Question


urlSubject : String
urlSubject =
    String.concat [ Config.baseUrl, "subjects/" ]


getSubject : Maybe String -> Http.Request (List Subject)
getSubject token =
    Http.request
        { method = "GET"
        , headers = (headerBuild token)
        , url = urlSubject
        , body = Http.emptyBody
        , expect = (Http.expectJson (Decode.list subjectDecoder))
        , timeout = Nothing
        , withCredentials = False
        }


fetchGetSubject : Maybe String -> Cmd Msg
fetchGetSubject token =
    Http.send OnFetchGetSubjects (getSubject token)



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



--Question Page


urlQuestionPage : PageNumber -> String
urlQuestionPage page =
    String.concat [ Config.baseUrl, "questions/?page=", (toString page) ]


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
    String.concat [ Config.baseUrl, "search/question/?page=", toString page ]


urlFilterSearchQuestion : String -> Filter -> String
urlFilterSearchQuestion baseUrl filters =
    let
        tags =
            filters.tags

        levelFilters =
            filters.levelFilters

        subjectFilters =
            filters.subjectFilters

        educationLevelFilters =
            filters.educationLevelFilters
    in
        String.concat
            ([ baseUrl ]
                ++ (if List.length tags > 0 then
                        [ "&text__content=" ]
                            ++ List.map
                                (\t -> String.concat [ tagFormatter t, "," ])
                                tags
                    else
                        []
                   )
                ++ List.map
                    (\levelFilter ->
                        case levelFilter of
                            EasyLevel ->
                                "&level=Facil"

                            MediumLevel ->
                                "&level=Medio"

                            HardLevel ->
                                "&level=Dificil"

                            _ ->
                                ""
                    )
                    levelFilters
                ++ List.map
                    (\subjectFilter ->
                        String.concat [ "&subjects=", tagFormatter subjectFilter ]
                    )
                    subjectFilters
                ++ List.map
                    (\educationLevelFilter ->
                        String.concat [ "&education_level=", tagFormatter educationLevelFilter ]
                    )
                    educationLevelFilters
            )


urlSearch : PageNumber -> Filter -> String
urlSearch page filters =
    urlFilterSearchQuestion (urlBaseSearch page) filters


getQuestionFilterSearch : PageNumber -> Filter -> Maybe String -> Http.Request QuestionPage
getQuestionFilterSearch page filters token =
    Http.request
        { method = "GET"
        , headers = (headerBuild token)
        , url = (urlSearch page filters)
        , body = Http.emptyBody
        , expect = (Http.expectJson <| questionPageDecoder page)
        , timeout = Nothing
        , withCredentials = False
        }


fetchGetQuestionFilterSearch : PageNumber -> Filter -> Maybe String -> Cmd Msg
fetchGetQuestionFilterSearch page filters token =
    Http.send OnFetchGetQuestionFilterSearch (getQuestionFilterSearch page filters token)



-- Question List


urlQuestionList : Int -> String
urlQuestionList questionListId =
    if questionListId <= 0 then
        String.concat [ Config.baseUrl, "question_lists/" ]
    else
        String.concat [ Config.baseUrl, "question_lists/", toString questionListId, "/" ]


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



-- Question List Pages


urlListMineQuestionList : PageNumber -> String
urlListMineQuestionList page =
    String.concat [ Config.baseUrl, "question_lists/user_list_questions/?page=", (toString page) ]


getMineQuestionList : PageNumber -> Maybe String -> Http.Request (List QuestionList)
getMineQuestionList page token =
    Http.request
        { method = "GET"
        , headers = (headerBuild token)
        , url = (urlListMineQuestionList page)
        , body = Http.emptyBody
        , expect = (Http.expectJson <| Decode.list questionListDecoder)
        , timeout = Nothing
        , withCredentials = False
        }


fetchGetMineQuestionList : PageNumber -> Maybe String -> Cmd Msg
fetchGetMineQuestionList page token =
    Http.send OnFetchGetMineQuestionListPage (getMineQuestionList page token)



-- Question List (Docx file generation)


urlGenerateList : Int -> Model -> String
urlGenerateList id model =
    String.concat
        [ Config.baseUrl
        , "question_lists/"
        , toString id
        , "/generate_list/?answers="
        , toString model.generateWithAnswer
        , "&resolution="
        , toString model.generateWithResolution
        ]


getGenerateList : Int -> Maybe String -> Model -> Http.Request String
getGenerateList id token model =
    Http.request
        { method = "GET"
        , headers = (headerBuild token)
        , url = (urlGenerateList id model)
        , body = Http.emptyBody
        , expect = (Http.expectJson (field "code" Decode.string))
        , timeout = Nothing
        , withCredentials = False
        }


fetchGetGenerateList : Int -> Maybe String -> Model -> Cmd Msg
fetchGetGenerateList id token model =
    Http.send OnFecthQuestionListGenerate (getGenerateList id token model)
