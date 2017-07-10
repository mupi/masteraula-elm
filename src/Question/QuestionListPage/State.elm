module Question.QuestionListPage.State exposing (init, update)

import Http
import Material
import Navigation


-- My Modules

import App.Types as App
import Question.QuestionListPage.Types exposing (..)
import Question.QuestionListPage.Rest exposing (..)
import Question.QuestionList.State as QuestionList


-- initQuestionListPage : QuestionListPage
-- initQuestionListPage =
--     QuestionListPage 0 0 Nothing Nothing []


init : Model
init =
    Model
        []
        QuestionList.init
        ""
        Material.model


update : Msg -> Model -> App.Global -> ( Model, Cmd Msg )
update msg model global =
    case msg of
        GetMineQuestionListPage ->
            model ! [ fetchGetMineQuestionList global.token ]

        QuestionListClick questionList ->
            model ! [ Navigation.newUrl <| String.concat [ "#questions/questionlists/", toString questionList.id ] ]

        QuestionListMsg subMsg ->
            let
                ( updatedQuestionList, cmd ) =
                    QuestionList.update subMsg model.questionList global
            in
                { model | questionList = updatedQuestionList } ! [ Cmd.map QuestionListMsg cmd ]

        OnFetchGetMineQuestionListPage (Ok questionLists) ->
            { model | questionLists = questionLists, error = "" } ! []

        OnFetchGetMineQuestionListPage (Err error) ->
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
                { model | error = errorMsg } ! []

        NoOp ->
            model ! []

        Mdl msg_ ->
            Material.update Mdl msg_ model
