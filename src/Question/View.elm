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
         -- ++ if model.selectingQuestions then
         --     filters model
         --    else
         --     []
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
            div [] [ text "1" ]

        QuestionPageRoute pageNumber ->
            div [] [ text "2" ]

        QuestionListRoute ->
            Html.map QuestionListEditMsg <| QuestionListEdit.view model.questionListEdit

        SelectedQuestionListRoute questionId ->
            div [] [ text "4" ]

        UserQuestionListRoute page ->
            div [] [ text "5" ]

        QuestionTagSearchRoute page ->
            div [] [ text "6" ]
