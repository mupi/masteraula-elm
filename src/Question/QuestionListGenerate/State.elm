module Question.QuestionListGenerate.State exposing (init, update)

import App.Config as Config
import Http
import Navigation
import Material


-- My Modules

import App.Types as App
import Question.QuestionListGenerate.Types exposing (..)
import Question.QuestionListGenerate.Rest exposing (..)


init : Model
init =
    Model
        False
        False
        False
        False
        ""
        Material.model


update : Msg -> Model -> App.Global -> ( Model, Cmd Msg )
update msg model global =
    case msg of
        QuestionListGenerate questionList ->
            -- { model | downloading = True, generateWithAnswer = False, generateWithResolution = False } ! [ fetchGetGenerateList questionList.id global.token model ]
            { model | generateWithAnswer = False, generateWithResolution = False, downloading = True } ! [ fetchGetGenerateList questionList.id global.token model ]

        ToggleGenerateWithAnswer ->
            { model | generateWithAnswer = not model.generateWithAnswer } ! []

        ToggleGenerateWithResolution ->
            { model | generateWithResolution = not model.generateWithResolution } ! []

        OnFecthQuestionListGenerate (Ok id) ->
            let
                newUrl =
                    String.concat [ Config.baseUrl, "question_lists/", id, "/get_list/" ]
            in
                { model | error = "", downloading = False } ! [ Navigation.load newUrl ]

        OnFecthQuestionListGenerate (Err error) ->
            let
                errorMsg =
                    case error of
                        Http.NetworkError ->
                            "Erro de conexão com o servidor"

                        Http.BadStatus response ->
                            "Página inexistente"

                        _ ->
                            toString error
            in
                { model | error = errorMsg, downloading = False } ! []

        NoOp ->
            model ! []

        Mdl msg_ ->
            Material.update Mdl msg_ model
