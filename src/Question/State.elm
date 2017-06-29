module Question.State exposing (init, update)

import Navigation
import Material


-- My Modules

import App.Types as App
import App.Ports as App
import Question.Types exposing (..)
import Question.Routing exposing (..)
import Question.Question.Rest as Question
import Question.Question.Types as Question
import Question.QuestionListEdit.State as QuestionListEdit


init : Model
init =
    Model
        QuestionListEdit.init
        (QuestionRoute 0)
        False
        False
        ""
        Material.model


update : Msg -> Model -> App.Global -> ( Model, Cmd Msg )
update msg model global =
    case msg of
        QuestionListEditMsg subMsg ->
            let
                ( updatedQuestionListEdit, cmd ) =
                    QuestionListEdit.update subMsg model.questionListEdit global
            in
                { model | questionListEdit = updatedQuestionListEdit } ! [ Cmd.map QuestionListEditMsg cmd ]

        DrawerLinkClick drawerLink ->
            case drawerLink of
                MineLists ->
                    model ! [ Navigation.newUrl "#questions/user_lists/1" ]

                SelectQuestions ->
                    model ! [ Navigation.newUrl "#questions/1" ]

                SelectedQuestions ->
                    model ! [ Navigation.newUrl "#questions/questionlist/" ]

        OnLocationChange route ->
            { model | route = route } ! []

        -- case model.route of
        -- QuestionRoute questionId ->
        --
        -- QuestionPageRoute pageNumber ->
        --
        -- QuestionListRoute ->
        --
        -- SelectedQuestionListRoute questionId ->
        --
        -- UserQuestionListRoute page ->
        --
        -- QuestionTagSearchRoute page ->
        NoOp ->
            model ! []

        Mdl msg_ ->
            Material.update Mdl msg_ model
