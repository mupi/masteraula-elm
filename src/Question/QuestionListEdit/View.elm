module Question.QuestionListEdit.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, placeholder, autofocus, value, name, id)
import Html.Events exposing (..)
import Markdown
import Material.Button as Button
import Material.Card as Card
import Material.Color as Color
import Material.Dialog as Dialog
import Material.Grid as Grid
import Material.Grid exposing (grid, cell, size, offset, Device(..))
import Material.Icon as Icon
import Material.Options as Options exposing (css)
import Material.Snackbar as Snackbar


-- My modules

import Question.QuestionListEdit.Types exposing (..)
import Question.Question.Types as Question
import Question.QuestionList.Types as QuestionList
import Utils.StringUtils as StringUtils


-- Dialog


dialog : Model -> Html Msg
dialog model =
    case model.dialog of
        Delete ->
            Dialog.view
                [ Options.cs "question_dialog"
                ]
                [ Dialog.title []
                    [ text "Apagar Lista"
                    ]
                , Dialog.content []
                    [ p [] [ text "Deseja mesmo apagar a lista de questões?" ]
                    , p [] [ text "Esta ação não poderá ser desfeita." ]
                    ]
                , Dialog.actions []
                    [ Button.render Mdl
                        [ 0 ]
                        model.mdl
                        [ Dialog.closeOn "click"
                        , Options.onClick <| QuestionListMsg QuestionList.QuestionListDelete
                        ]
                        [ text "Confirmar" ]
                    , Button.render Mdl
                        [ 1 ]
                        model.mdl
                        [ Dialog.closeOn "click" ]
                        [ text "Cancelar" ]
                    ]
                ]

        Clear ->
            Dialog.view
                [ Options.cs "question_dialog"
                ]
                [ Dialog.title []
                    [ text "Limpar Lista"
                    ]
                , Dialog.content []
                    [ p [] [ text "Deseja remover todos os exercícios desta lista de questões?" ]
                    , p [] [ text "Esta ação não poderá ser desfeita." ]
                    ]
                , Dialog.actions []
                    [ Button.render Mdl
                        [ 0 ]
                        model.mdl
                        [ Dialog.closeOn "click"
                        , Options.onClick (QuestionListMsg QuestionList.QuestionListClear)
                        ]
                        [ text "Confirmar" ]
                    , Button.render Mdl
                        [ 1 ]
                        model.mdl
                        [ Dialog.closeOn "click" ]
                        [ text "Cancelar" ]
                    ]
                ]


view : Model -> Html Msg
view model =
    div []
        [ viewQuestionList model
        , Snackbar.view model.snackbar |> Html.map Snackbar
        , dialog model
        ]


cardTitle : Question.Question -> Card.Block msg
cardTitle question =
    Card.title
        [ Options.css "height" "80px"
        ]
        [ Options.div
            []
            [ Card.subhead
                []
                [ if question.credit_cost == 0 then
                    text "Questão Gratuita!"
                  else if question.credit_cost == 1 then
                    toString question.credit_cost
                        ++ " Crédito"
                        |> text
                  else
                    toString question.credit_cost
                        ++ " Créditos"
                        |> text
                ]
            , Card.head []
                [ text <|
                    if (List.length question.subjects > 1) then
                        "Multidisciplinar"
                    else if (List.length question.subjects <= 0) then
                        ""
                    else
                        case List.head question.subjects of
                            Just s ->
                                s.subject_name

                            Nothing ->
                                ""
                ]
            ]
        ]



-- Question card view


questionCardButton : Model -> Question.Question -> Card.Block Msg
questionCardButton model question =
    let
        questionsId =
            List.map (\q -> q.question.id) model.questionList.questionList.questions
    in
        Card.actions
            [ Card.border
            , Color.background (Color.color Color.Red Color.S700)
            , Options.css "height" "52px"
            ]
            [ Button.render Mdl
                [ 2, 1, question.id ]
                model.mdl
                [ Button.ripple
                , Button.accent
                , Options.onClick <| QuestionListMsg (QuestionList.QuestionListRemove question)
                , Color.text Color.white
                , css "font-size" "11px"
                , css "width" "100%"
                ]
                [ Icon.view "remove" [ Icon.size18 ], text " Remover" ]
            ]


questionCardView : Model -> Question.Question -> Bool -> Html Msg
questionCardView model question forceLoad =
    let
        year_text =
            StringUtils.maybeIntToString question.year

        source_text =
            StringUtils.maybeStringToString question.source
    in
        Card.view
            [ Color.background (Color.white)
            , css "width" "100%"
            , Options.cs "mdl-shadow--2dp"
            , Options.onClick (QuestionClick question)
            ]
            [ cardTitle question
            , Card.text
                [ css "height" "196px"
                , Options.cs "question_card thumb"
                ]
                [ (if (String.length year_text > 0) && (String.length source_text > 0) then
                    text ("(" ++ year_text ++ " - " ++ source_text ++ ")")
                   else if String.length year_text > 0 then
                    text ("(" ++ year_text ++ ")")
                   else if String.length source_text > 0 then
                    text ("(" ++ source_text ++ ")")
                   else
                    text ""
                  )
                , Markdown.toHtml [] question.question_statement
                ]
            , (questionCardButton model question)
            ]


viewQuestionList : Model -> Html Msg
viewQuestionList model =
    let
        questionList =
            model.questionList.questionList
    in
        Options.div
            []
            [ input
                [ id "new-list-header"
                , class "new-list-header"
                , autofocus True
                , placeholder "Nome da nova lista"
                , value questionList.question_list_header
                , onInput QuestionListHeaderInput
                ]
                []
            , Options.styled p
                [ Options.css "margin" "0 20px"
                ]
                [ text "Veja abaixo as questões que você selecionou. Para baixá-las, digite um nome para a lista no campo acima, clique em salvar e em seguida Fazer Download." ]
            , Grid.grid [ Options.cs "questions_list_display" ]
                (List.map
                    (\question ->
                        Grid.cell
                            [ size All 3
                            , Options.css "padding" "8px 8px"
                            ]
                            [ questionCardView model question False ]
                    )
                    (List.map (\q -> q.question) questionList.questions)
                    ++ [ Grid.cell
                            [ size All 3
                            , Options.css "padding" "8px 8px"
                            ]
                            [ Card.view
                                [ Color.background (Color.white)
                                , css "width" "100%"
                                , Options.cs "mdl-shadow--2dp"
                                , Options.onClick SelectQuestions
                                ]
                                [ Card.text
                                    [ css "height" "328px"
                                    , Options.css "line-height" "328px"
                                    , Options.css "text-align" "center"
                                    , Options.cs "question_card thumb"
                                    ]
                                    [ Options.span
                                        [ Options.css "display" "inline-block"
                                        , Options.css "vertical-align" "middle"
                                        , Options.css "line-height" "normal"
                                        ]
                                        [ Icon.view "add" [ Options.css "font-size" "128px" ] ]
                                    ]
                                ]
                            ]
                       ]
                )
            , if questionList.id == 0 then
                viewQuestionListButtonNew model
              else
                viewQuestionListButtonEdit model
            ]


viewQuestionListButtonNew : Model -> Html Msg
viewQuestionListButtonNew model =
    let
        questions =
            model.questionList.questionList.questions
    in
        Options.div
            [ Color.background Color.primaryDark
            , Options.cs "questions_list_action"
            ]
            [ Button.render Mdl
                [ 5, 1 ]
                model.mdl
                [ Button.ripple
                , Button.plain
                , Color.text Color.white
                , Options.onClick <| QuestionListMsg QuestionList.QuestionListSave
                ]
                [ Icon.i "save", text "Salvar" ]
            , Button.render Mdl
                [ 5, 2 ]
                model.mdl
                [ Button.ripple
                , Button.plain
                , Color.text Color.white
                , Options.onClick (Dialog Clear)
                , if List.length questions > 0 then
                    Options.nop
                  else
                    Button.disabled
                , Options.onClick <| QuestionListMsg QuestionList.QuestionListClear
                ]
                [ Icon.i "delete_forever", text "Limpar lista" ]
            ]


viewQuestionListButtonEdit : Model -> Html Msg
viewQuestionListButtonEdit model =
    let
        questions =
            model.questionList.questionList.questions
    in
        Options.div
            [ Color.background Color.primaryDark
            , Options.cs "questions_list_action"
            ]
            [ Button.render Mdl
                [ 5, 1 ]
                model.mdl
                [ Button.ripple
                , Button.plain
                , Color.text Color.white
                , Options.onClick <| QuestionListMsg QuestionList.QuestionListSave
                ]
                [ Icon.i "save", text "Salvar" ]
            , Button.render Mdl
                [ 5, 2 ]
                model.mdl
                [ Button.ripple
                , Button.plain
                , Color.text Color.white
                , Options.onClick (Dialog Clear)
                , if List.length questions > 0 then
                    Options.nop
                  else
                    Button.disabled
                  -- , Options.onClick QuestionListClear
                ]
                [ Icon.i "delete_forever", text "Limpar lista" ]
            , Button.render Mdl
                [ 5, 3 ]
                model.mdl
                [ Button.ripple
                , Button.plain
                , Color.text Color.white
                , Options.onClick (Dialog Delete)
                , Options.onClick <| QuestionListMsg QuestionList.QuestionListDelete
                ]
                [ Icon.i "clear", text "Apagar lista" ]
            , Button.render Mdl
                [ 5, 4 ]
                model.mdl
                [ Button.ripple
                , Button.plain
                , Color.text Color.white
                , Options.onClick QuestionListCancel
                ]
                [ text "Cancelar" ]
            ]
