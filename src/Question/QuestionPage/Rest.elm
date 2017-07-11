module Question.QuestionPage.Rest exposing (..)

import Http
import Json.Decode as Decode exposing (field)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)


-- My modules

import App.Config as Config
import Question.QuestionPage.Types exposing (..)
import Question.Question.Rest as Question
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


headerBuild : Maybe String -> List Http.Header
headerBuild token =
    case token of
        Just token ->
            [ Http.header "Authorization" (String.concat [ "JWT ", token ]) ]

        Nothing ->
            []



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



-- Subjects


fetchGetSubject : Maybe String -> Cmd Msg
fetchGetSubject token =
    Http.send OnFetchGetSubjects (Question.getSubject token)



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
