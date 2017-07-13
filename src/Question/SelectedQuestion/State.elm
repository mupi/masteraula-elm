module Question.SelectedQuestion.State exposing (init, update)

import Navigation
import Material


-- My Modules

import App.Types as App
import Question.SelectedQuestion.Types exposing (..)
import Question.Question.State as Question
import Question.Question.Types as Question
import Question.QuestionList.State as QuestionList


init : Model
init =
    Model
        Question.init
        QuestionList.init
        False
        ""
        Material.model


update : Msg -> Model -> App.Global -> ( Model, Cmd Msg )
update msg model global =
    case msg of
        GetQuestion questionId ->
            let
                ( updatedQuestion, cmd ) =
                    Question.update (Question.GetQuestion questionId) model.question global
            in
                { model | question = updatedQuestion, loading = True } ! [ Cmd.map QuestionMsg cmd ]

        QuestionClick question ->
            let
                questionModel =
                    model.question

                newQuestion =
                    { questionModel | question = question }
            in
                { model | question = newQuestion } ! []

        QuestionBack ->
            model ! [ Navigation.back 1 ]

        UpdateQuestionList questionList ->
            { model | questionList = questionList } ! []

        QuestionListMsg subMsg ->
            let
                ( updatedQuestionList, cmd ) =
                    QuestionList.update subMsg model.questionList global
            in
                { model | questionList = updatedQuestionList } ! [ Cmd.map QuestionListMsg cmd ]

        QuestionMsg subMsg ->
            let
                ( updatedQuestion, cmd ) =
                    Question.update subMsg model.question global
            in
                { model | question = updatedQuestion, loading = False } ! [ Cmd.map QuestionMsg cmd ]

        NoOp ->
            model ! []

        Mdl msg_ ->
            Material.update Mdl msg_ model
