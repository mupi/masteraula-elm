module Question.View exposing (..)

import Html exposing (..)
import Material.Icon as Icon
import Material.Layout as Layout
import Material.Options as Options exposing (css)
import Material.Spinner as Loading


-- My modules

import Question.Types exposing (..)
import Question.Routing exposing (..)
import Question.QuestionListEdit.View as QuestionListEdit
import Question.SelectedQuestion.View as SelectedQuestion
import Question.SelectedQuestionList.View as SelectedQuestionList
import Question.QuestionListPage.View as QuestionListPage
import Question.QuestionPage.View as QuestionPage


-- import Question.Question.Types as Question
-- import Question.QuestionList.Types as QuestionList
-- import Utils.StringUtils as StringUtils


drawerLink : Model -> Html Msg
drawerLink model =
    Layout.navigation
        []
        ([ Layout.link
            [ Options.onClick <| DrawerLinkClick SelectedQuestions
            ]
            [ Icon.view "list" [ Icon.size18 ], text " Preparar prova" ]
         , Layout.link
            [ Options.onClick <| DrawerLinkClick MineLists
            ]
            [ Icon.view "favorite" [ Icon.size18 ]
            , text " Minhas provas"
            ]
         , Layout.link
            [ Options.onClick <| DrawerLinkClick SelectQuestions
            ]
            [ Icon.view "view_module" [ Icon.size18 ], text " Banco de questões" ]
         ]
            ++ (case model.route of
                    QuestionPageRoute _ ->
                        (List.map (Html.map QuestionPageMsg) <| QuestionPage.filters model.questionPage)

                    QuestionTagSearchRoute _ ->
                        (List.map (Html.map QuestionPageMsg) <| QuestionPage.filters model.questionPage)

                    _ ->
                        []
               )
        )



-- Dialog


view : Model -> Html Msg
view model =
    div []
        [ (if model.loading then
            Options.div [ Options.cs "question_loader_div" ] [ Options.div [ Options.cs "question_loader" ] [ Loading.spinner [ Loading.active model.loading ] ] ]
           else
            span [] []
          )
        , page model
        ]


page : Model -> Html Msg
page model =
    case model.route of
        QuestionRoute questionId ->
            Html.map SelectedQuestionMsg <| SelectedQuestion.view model.selectedQuestion

        QuestionPageRoute pageNumber ->
            Html.map QuestionPageMsg <| QuestionPage.view model.questionPage

        QuestionListRoute ->
            Html.map QuestionListEditMsg <| QuestionListEdit.view model.questionListEdit

        SelectedQuestionListRoute questionListId ->
            Html.map SelectedQuestionListMsg <| SelectedQuestionList.view model.selectedQuestionList

        MineQuestionListsRoute ->
            Html.map QuestionListPageMsg <| QuestionListPage.view model.questionListPage

        QuestionTagSearchRoute page ->
            Html.map QuestionPageMsg <| QuestionPage.view model.questionPage
