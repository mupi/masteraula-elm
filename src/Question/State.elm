module Question.State exposing (init, update)

import Navigation
import Material


-- My Modules

import App.Types as App
import Question.Types exposing (..)
import Question.Routing exposing (..)
import Question.QuestionList.Types as QuestionList
import Question.QuestionListEdit.State as QuestionListEdit
import Question.QuestionListEdit.Types as QuestionListEdit
import Question.SelectedQuestion.State as SelectedQuestion
import Question.SelectedQuestion.Types as SelectedQuestion
import Question.SelectedQuestionList.State as SelectedQuestionList
import Question.SelectedQuestionList.Types as SelectedQuestionList
import Question.QuestionListPage.Types as QuestionListPage
import Question.QuestionListPage.State as QuestionListPage


init : Model
init =
    Model
        QuestionListEdit.init
        SelectedQuestion.init
        SelectedQuestionList.init
        QuestionListPage.init
        (QuestionRoute 0)
        False
        False
        ""
        Material.model


updateQuestionList : Model -> App.Global -> QuestionList.Model -> ( QuestionListEdit.Model, SelectedQuestion.Model, SelectedQuestionList.Model )
updateQuestionList model global questionList =
    let
        ( updatedQuestionListEdit, _ ) =
            QuestionListEdit.update (QuestionListEdit.UpdateQuestionList questionList) model.questionListEdit global

        ( updatedSelectedQuestion, _ ) =
            SelectedQuestion.update (SelectedQuestion.UpdateQuestionList questionList) model.selectedQuestion global

        ( updatedSelectedQuestionList, _ ) =
            SelectedQuestionList.update (SelectedQuestionList.UpdateQuestionList questionList) model.selectedQuestionList global
    in
        ( updatedQuestionListEdit, updatedSelectedQuestion, updatedSelectedQuestionList )


update : Msg -> Model -> App.Global -> ( Model, Cmd Msg )
update msg model global =
    case msg of
        QuestionListEditMsg subMsg ->
            let
                ( updatedQuestionListEdit, cmd ) =
                    QuestionListEdit.update subMsg model.questionListEdit global

                ( _, updatedSelectedQuestion, updatedSelectedQuestionList ) =
                    updateQuestionList model global updatedQuestionListEdit.questionList
            in
                if
                    subMsg
                        == (QuestionListEdit.QuestionListMsg QuestionList.QuestionListSave)
                        || subMsg
                        == (QuestionListEdit.QuestionListMsg QuestionList.QuestionListDelete)
                then
                    { model
                        | questionListEdit = updatedQuestionListEdit
                        , selectedQuestion = updatedSelectedQuestion
                        , selectedQuestionList = updatedSelectedQuestionList
                    }
                        ! [ Cmd.map QuestionListEditMsg cmd ]
                else
                    { model
                        | questionListEdit = updatedQuestionListEdit
                    }
                        ! [ Cmd.map QuestionListEditMsg cmd ]

        SelectedQuestionMsg subMsg ->
            let
                ( updatedSelectedQuestion, cmd ) =
                    SelectedQuestion.update subMsg model.selectedQuestion global

                ( updatedQuestionListEdit, _, updatedSelectedQuestionList ) =
                    updateQuestionList model global updatedSelectedQuestion.questionList
            in
                { model
                    | questionListEdit = updatedQuestionListEdit
                    , selectedQuestion = updatedSelectedQuestion
                    , selectedQuestionList = updatedSelectedQuestionList
                }
                    ! [ Cmd.map SelectedQuestionMsg cmd ]

        SelectedQuestionListMsg subMsg ->
            let
                ( updatedSelectedQuestionList, cmd ) =
                    SelectedQuestionList.update subMsg model.selectedQuestionList global

                ( updatedQuestionListEdit, updatedSelectedQuestion, _ ) =
                    updateQuestionList model global updatedSelectedQuestionList.questionList
            in
                if subMsg == SelectedQuestionList.QuestionListEdit then
                    { model
                        | questionListEdit = updatedQuestionListEdit
                        , selectedQuestion = updatedSelectedQuestion
                        , selectedQuestionList = updatedSelectedQuestionList
                    }
                        ! [ Cmd.map SelectedQuestionListMsg cmd ]
                else
                    { model
                        | selectedQuestionList = updatedSelectedQuestionList
                    }
                        ! [ Cmd.map SelectedQuestionListMsg cmd ]

        QuestionListPageMsg subMsg ->
            let
                ( updatedQuestionListPage, cmd ) =
                    QuestionListPage.update subMsg model.questionListPage global

                -- ( _, _ ) =
                --     updateQuestionList model global updatedQuestionListPage.questionList
            in
                { model
                    | questionListPage = updatedQuestionListPage
                }
                    ! [ Cmd.map QuestionListPageMsg cmd ]

        DrawerLinkClick drawerLink ->
            case drawerLink of
                MineLists ->
                    model ! [ Navigation.newUrl "#questions/user_lists" ]

                SelectQuestions ->
                    model ! [ Navigation.newUrl "#questions/1" ]

                SelectedQuestions ->
                    model ! [ Navigation.newUrl "#questions/questionlist/" ]

        OnLocationChange route ->
            if model.route == route then
                model ! []
            else
                case route of
                    QuestionRoute questionId ->
                        let
                            ( updatedQuestion, cmd ) =
                                SelectedQuestion.update (SelectedQuestion.GetQuestion questionId) model.selectedQuestion global
                        in
                            { model | route = route } ! [ Cmd.map SelectedQuestionMsg cmd ]

                    QuestionPageRoute pageNumber ->
                        { model | route = route } ! []

                    QuestionListRoute ->
                        { model | route = route } ! []

                    SelectedQuestionListRoute questionListId ->
                        let
                            ( updatedQuestion, cmd ) =
                                SelectedQuestionList.update (SelectedQuestionList.GetQuestionList questionListId) model.selectedQuestionList global
                        in
                            { model | route = route } ! [ Cmd.map SelectedQuestionListMsg cmd ]

                    MineQuestionListsRoute ->
                        let
                            ( updatedQuestionListPage, cmd ) =
                                QuestionListPage.update QuestionListPage.GetMineQuestionListPage model.questionListPage global
                        in
                            { model | route = route } ! [ Cmd.map QuestionListPageMsg cmd ]

                    QuestionTagSearchRoute page ->
                        { model | route = route } ! []

        NoOp ->
            model ! []

        Mdl msg_ ->
            Material.update Mdl msg_ model
