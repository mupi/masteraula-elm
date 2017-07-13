module Question.SelectedQuestionList.State exposing (init, update)

import Navigation
import Material
import Material.Snackbar as Snackbar
import Material.Helpers exposing (map1st, map2nd)


-- My Modules

import App.Types as App
import Question.SelectedQuestionList.Types exposing (..)
import Question.QuestionList.State as QuestionList
import Question.QuestionList.Types as QuestionList
import Question.QuestionListGenerate.State as QuestionListGenerate


init : Model
init =
    Model
        QuestionList.init
        QuestionListGenerate.init
        False
        Snackbar.model
        Material.model


update : Msg -> Model -> App.Global -> ( Model, Cmd Msg )
update msg model global =
    case msg of
        GetQuestionList questionListId ->
            let
                ( updatedQuestionList, cmd ) =
                    QuestionList.update (QuestionList.GetQuestionList questionListId) model.questionList global
            in
                { model | questionList = updatedQuestionList, loading = True } ! [ Cmd.map QuestionListMsg cmd ]

        QuestionListEdit ->
            model ! [ Navigation.newUrl <| String.concat [ "#questions/questionlist" ] ]

        UpdateQuestionList questionList ->
            { model | questionList = questionList } ! []

        QuestionClick question ->
            model ! [ Navigation.newUrl <| String.concat [ "#question/", toString question.id ] ]

        QuestionListMsg subMsg ->
            let
                ( updatedQuestionList, cmd ) =
                    QuestionList.update subMsg model.questionList global
            in
                { model | questionList = updatedQuestionList, loading = False } ! [ Cmd.map QuestionListMsg cmd ]

        QuestionListGenerateMsg subMsg ->
            let
                ( updatedQuestionListGenerate, cmd ) =
                    QuestionListGenerate.update subMsg model.questionListGenerate global
            in
                model ! [ Cmd.map QuestionListGenerateMsg cmd ]

        Dialog ->
            model ! []

        NoOp ->
            model ! []

        Snackbar (Snackbar.End a) ->
            { model | snackbar = Snackbar.model } ! []

        Snackbar msg ->
            Snackbar.update msg model.snackbar
                |> map1st (\s -> { model | snackbar = s })
                |> map2nd (Cmd.map Snackbar)

        Mdl msg_ ->
            Material.update Mdl msg_ model
