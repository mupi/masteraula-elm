module Question.QuestionListEdit.State exposing (init, update)

import Navigation
import Material
import Material.Snackbar as Snackbar
import Material.Helpers exposing (map1st, map2nd)


-- My Modules

import App.Types as App
import App.Ports as App
import Question.QuestionListEdit.Types exposing (..)
import Question.Question.Types as Question
import Question.Question.State as Question
import Question.QuestionList.Types as QuestionList
import Question.QuestionList.State as QuestionList
import Question.QuestionListGenerate.State as QuestionListGenerate
import Question.QuestionListGenerate.Types as QuestionListGenerate


init : Model
init =
    Model
        QuestionList.init
        QuestionListGenerate.init
        ""
        Delete
        Snackbar.model
        Material.model


update : Msg -> Model -> App.Global -> ( Model, Cmd Msg )
update msg model global =
    case msg of
        QuestionListEdit questionList ->
            let
                questionListModel =
                    model.questionList

                newQuestionListModel =
                    { questionListModel | questionList = questionList }
            in
                { model | questionList = newQuestionListModel } ! []

        QuestionListHeaderInput newQuestionHeader ->
            let
                ( updatedQuestionList, cmd ) =
                    QuestionList.update (QuestionList.QuestionListHeaderInput newQuestionHeader) model.questionList global
            in
                { model | questionList = updatedQuestionList } ! []

        QuestionListCancel ->
            let
                questionListId =
                    model.questionList.questionList.id
            in
                { model | questionList = QuestionList.init } ! [ Navigation.newUrl <| String.concat [ "#questions/questionlists/", toString questionListId ] ]

        SelectQuestions ->
            model ! [ Navigation.newUrl "#questions/1" ]

        UpdateQuestionList questionList ->
            { model | questionList = questionList } ! []

        QuestionListMsg questionListMsg ->
            let
                ( updatedQuestionList, cmd ) =
                    QuestionList.update questionListMsg model.questionList global

                ( snackbar, effect ) =
                    Snackbar.add (Snackbar.snackbar 0 updatedQuestionList.error "Close") model.snackbar
                        |> map2nd (Cmd.map Snackbar)
            in
                if updatedQuestionList.error /= "" then
                    { model | questionList = updatedQuestionList, snackbar = snackbar } ! [ effect ]
                else
                    { model | questionList = updatedQuestionList } ! [ Cmd.map QuestionListMsg cmd ]

        QuestionClick question ->
            model ! [ Navigation.newUrl <| String.concat [ "#question/", toString question.id ] ]

        Dialog dialog ->
            { model | dialog = dialog } ! [ App.displayDialog "" ]

        Snackbar (Snackbar.End a) ->
            { model | snackbar = Snackbar.model } ! []

        Snackbar msg ->
            Snackbar.update msg model.snackbar
                |> map1st (\s -> { model | snackbar = s })
                |> map2nd (Cmd.map Snackbar)

        NoOp ->
            model ! []

        Mdl msg_ ->
            Material.update Mdl msg_ model
