module Question.Rest exposing (..)

import Http
import Json.Decode as Decode exposing (field)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)


-- My modules

import App.Config as Config
import Question.Types exposing (..)
import Question.Question.Types as Question
import Question.Question.Rest as Question
import Question.QuestionList.Rest as QuestionList
import Question.QuestionList.Types as QuestionList
import Utils.StringUtils exposing (..)


-- Decoders


questionPageDecoder : Int -> Decode.Decoder QuestionPage
questionPageDecoder page =
    decode QuestionPage
        |> required "count" Decode.int
        |> hardcoded page
        |> required "next" (Decode.nullable Decode.string)
        |> required "previous" (Decode.nullable Decode.string)
        |> required "results" (Decode.list Question.questionDecoder)


questionListPageDecoder : Int -> Decode.Decoder QuestionListPage
questionListPageDecoder page =
    decode QuestionListPage
        |> required "count" Decode.int
        |> hardcoded page
        |> required "next" (Decode.nullable Decode.string)
        |> required "previous" (Decode.nullable Decode.string)
        |> required "results" (Decode.list QuestionList.questionListDecoder)


headerBuild : Maybe String -> List Http.Header
headerBuild token =
    case token of
        Just token ->
            [ Http.header "Authorization" (String.concat [ "JWT ", token ]) ]

        Nothing ->
            []



-- Question


urlSubject : String
urlSubject =
    String.concat [ Config.baseUrl, "subjects/" ]


getSubject : Maybe String -> Http.Request (List Question.Subject)
getSubject token =
    Http.request
        { method = "GET"
        , headers = (headerBuild token)
        , url = urlSubject
        , body = Http.emptyBody
        , expect = (Http.expectJson (Decode.list Question.subjectDecoder))
        , timeout = Nothing
        , withCredentials = False
        }


fetchGetSubject : Maybe String -> Cmd Msg
fetchGetSubject token =
    Http.send OnFetchGetSubjects (getSubject token)



--Question Page


urlQuestionPage : Int -> String
urlQuestionPage page =
    String.concat [ Config.baseUrl, "questions/?page=", (toString page) ]


getQuestionPage : Int -> Maybe String -> Http.Request QuestionPage
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


fetchGetQuestionPage : Int -> Maybe String -> Cmd Msg
fetchGetQuestionPage page token =
    Http.send OnFetchGetQuestionPage (getQuestionPage page token)



-- Question Search


urlBaseSearch : Int -> String
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


urlSearch : Int -> Filter -> String
urlSearch page filters =
    urlFilterSearchQuestion (urlBaseSearch page) filters


getQuestionFilterSearch : Int -> Filter -> Maybe String -> Http.Request QuestionPage
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


fetchGetQuestionFilterSearch : Int -> Filter -> Maybe String -> Cmd Msg
fetchGetQuestionFilterSearch page filters token =
    Http.send OnFetchGetQuestionFilterSearch (getQuestionFilterSearch page filters token)



-- Question List Pages


urlListMineQuestionList : Int -> String
urlListMineQuestionList page =
    String.concat [ Config.baseUrl, "question_lists/user_list_questions/?page=", (toString page) ]


getMineQuestionList : Int -> Maybe String -> Http.Request (List QuestionList.QuestionList)
getMineQuestionList page token =
    Http.request
        { method = "GET"
        , headers = (headerBuild token)
        , url = (urlListMineQuestionList page)
        , body = Http.emptyBody
        , expect = (Http.expectJson <| Decode.list QuestionList.questionListDecoder)
        , timeout = Nothing
        , withCredentials = False
        }


fetchGetMineQuestionList : Int -> Maybe String -> Cmd Msg
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
