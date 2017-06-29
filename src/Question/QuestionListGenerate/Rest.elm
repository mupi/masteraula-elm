module Question.QuestionListGenerate.Rest exposing (..)

import Http
import Json.Decode as Decode exposing (field)


-- My modules

import App.Config as Config
import Question.QuestionListGenerate.Types exposing (..)


headerBuild : Maybe String -> List Http.Header
headerBuild token =
    case token of
        Just token ->
            [ Http.header "Authorization" (String.concat [ "JWT ", token ]) ]

        Nothing ->
            []



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
