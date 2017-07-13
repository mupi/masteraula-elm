module Question.State exposing (init, initStorage, update)

import Navigation
import Material


-- My Modules

import App.Types as App
import App.Ports as Ports
import Question.Types exposing (..)
import Question.Routing exposing (..)
import Question.QuestionList.Types as QuestionList
import Question.QuestionList.State as QuestionList
import Question.QuestionListEdit.State as QuestionListEdit
import Question.QuestionListEdit.Types as QuestionListEdit
import Question.SelectedQuestion.State as SelectedQuestion
import Question.SelectedQuestion.Types as SelectedQuestion
import Question.SelectedQuestionList.State as SelectedQuestionList
import Question.SelectedQuestionList.Types as SelectedQuestionList
import Question.QuestionListPage.Types as QuestionListPage
import Question.QuestionListPage.State as QuestionListPage
import Question.QuestionPage.Types as QuestionPage
import Question.QuestionPage.State as QuestionPage


init : Model
init =
    Model
        QuestionListEdit.init
        SelectedQuestion.init
        SelectedQuestionList.init
        QuestionListPage.init
        QuestionPage.init
        (QuestionRoute 0)
        False
        ""
        Material.model


initStorage : QuestionList.QuestionList -> Model
initStorage questionList =
    let
        questionListModel =
            QuestionList.init

        newQuestionListModel =
            { questionListModel | questionList = questionList }

        questionListEdit =
            QuestionListEdit.init

        newQuestionListEdit =
            { questionListEdit | questionList = newQuestionListModel }

        questionPage =
            QuestionPage.init

        newQuestionPage =
            { questionPage | questionList = newQuestionListModel }
    in
        Model
            newQuestionListEdit
            SelectedQuestion.init
            SelectedQuestionList.init
            QuestionListPage.init
            newQuestionPage
            (QuestionRoute 0)
            False
            ""
            Material.model


updateQuestionList : Model -> App.Global -> QuestionList.Model -> ( QuestionListEdit.Model, SelectedQuestion.Model, SelectedQuestionList.Model, QuestionPage.Model )
updateQuestionList model global questionList =
    let
        ( updatedQuestionListEdit, _ ) =
            QuestionListEdit.update (QuestionListEdit.UpdateQuestionList questionList) model.questionListEdit global

        ( updatedSelectedQuestion, _ ) =
            SelectedQuestion.update (SelectedQuestion.UpdateQuestionList questionList) model.selectedQuestion global

        ( updatedSelectedQuestionList, _ ) =
            SelectedQuestionList.update (SelectedQuestionList.UpdateQuestionList questionList) model.selectedQuestionList global

        ( updatedQuestionPage, _ ) =
            QuestionPage.update (QuestionPage.UpdateQuestionList questionList) model.questionPage global
    in
        ( updatedQuestionListEdit, updatedSelectedQuestion, updatedSelectedQuestionList, updatedQuestionPage )


update : Msg -> Model -> App.Global -> ( Model, Cmd Msg )
update msg model global =
    case msg of
        QuestionListEditMsg subMsg ->
            let
                ( updatedQuestionListEdit, cmd ) =
                    QuestionListEdit.update subMsg model.questionListEdit global

                ( _, updatedSelectedQuestion, updatedSelectedQuestionList, updatedQuestionPage ) =
                    updateQuestionList model global updatedQuestionListEdit.questionList
            in
                if
                    subMsg
                        == (QuestionListEdit.QuestionListMsg QuestionList.QuestionListSave)
                        || subMsg
                        == (QuestionListEdit.QuestionListMsg QuestionList.QuestionListDelete)
                        || subMsg
                        == QuestionListEdit.QuestionListCancel
                then
                    { model
                        | questionListEdit = updatedQuestionListEdit
                        , selectedQuestion = updatedSelectedQuestion
                        , selectedQuestionList = updatedSelectedQuestionList
                        , questionPage = updatedQuestionPage
                    }
                        ! [ Cmd.map QuestionListEditMsg cmd ]
                else
                    { model
                        | questionListEdit = updatedQuestionListEdit
                    }
                        ! [ Cmd.map QuestionListEditMsg cmd, Ports.setLocalStorageQuestionList updatedQuestionListEdit.questionList.questionList ]

        SelectedQuestionMsg subMsg ->
            let
                ( updatedSelectedQuestion, cmd ) =
                    SelectedQuestion.update subMsg model.selectedQuestion global

                ( updatedQuestionListEdit, _, updatedSelectedQuestionList, updatedQuestionPage ) =
                    updateQuestionList model global updatedSelectedQuestion.questionList
            in
                { model
                    | questionListEdit = updatedQuestionListEdit
                    , selectedQuestion = updatedSelectedQuestion
                    , selectedQuestionList = updatedSelectedQuestionList
                    , questionPage = updatedQuestionPage
                }
                    ! [ Cmd.map SelectedQuestionMsg cmd ]

        SelectedQuestionListMsg subMsg ->
            let
                ( updatedSelectedQuestionList, cmd ) =
                    SelectedQuestionList.update subMsg model.selectedQuestionList global

                ( updatedQuestionListEdit, updatedSelectedQuestion, _, updatedQuestionPage ) =
                    updateQuestionList model global updatedSelectedQuestionList.questionList
            in
                if subMsg == SelectedQuestionList.QuestionListEdit then
                    { model
                        | questionListEdit = updatedQuestionListEdit
                        , selectedQuestion = updatedSelectedQuestion
                        , selectedQuestionList = updatedSelectedQuestionList
                        , questionPage = updatedQuestionPage
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

        QuestionPageMsg subMsg ->
            let
                ( updatedQuestionPage, cmd ) =
                    QuestionPage.update subMsg model.questionPage global

                ( updatedQuestionListEdit, updatedSelectedQuestion, updatedQuestionListPage, _ ) =
                    updateQuestionList model global updatedQuestionPage.questionList
            in
                case subMsg of
                    QuestionPage.QuestionListMsg (QuestionList.QuestionListAdd _) ->
                        { model
                            | questionListEdit = updatedQuestionListEdit
                            , selectedQuestion = updatedSelectedQuestion
                            , selectedQuestionList = updatedQuestionListPage
                            , questionPage = updatedQuestionPage
                        }
                            ! [ Cmd.map QuestionPageMsg cmd, Ports.setLocalStorageQuestionList updatedQuestionPage.questionList.questionList ]

                    _ ->
                        { model
                            | questionPage = updatedQuestionPage
                        }
                            ! [ Cmd.map QuestionPageMsg cmd ]

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
                            ( updatedSelectedQuestion, cmd ) =
                                SelectedQuestion.update (SelectedQuestion.GetQuestion questionId) model.selectedQuestion global
                        in
                            { model | selectedQuestion = updatedSelectedQuestion, route = route } ! [ Cmd.map SelectedQuestionMsg cmd ]

                    QuestionPageRoute pageNumber ->
                        let
                            ( updatedQuestionPage, cmd ) =
                                QuestionPage.update (QuestionPage.GetQuestionPage pageNumber) model.questionPage global
                        in
                            { model | questionPage = updatedQuestionPage, route = route } ! [ Cmd.map QuestionPageMsg cmd ]

                    QuestionListRoute ->
                        { model | route = route } ! []

                    SelectedQuestionListRoute questionListId ->
                        let
                            ( updatedSelectedQuestionList, cmd ) =
                                SelectedQuestionList.update (SelectedQuestionList.GetQuestionList questionListId) model.selectedQuestionList global
                        in
                            { model | selectedQuestionList = updatedSelectedQuestionList, route = route } ! [ Cmd.map SelectedQuestionListMsg cmd ]

                    MineQuestionListsRoute ->
                        let
                            ( updatedMineQuestionList, cmd ) =
                                QuestionListPage.update QuestionListPage.GetMineQuestionListPage model.questionListPage global
                        in
                            { model | questionListPage = updatedMineQuestionList, route = route } ! [ Cmd.map QuestionListPageMsg cmd ]

                    QuestionTagSearchRoute page ->
                        { model | route = route } ! []

        NoOp ->
            model ! []

        Mdl msg_ ->
            Material.update Mdl msg_ model
