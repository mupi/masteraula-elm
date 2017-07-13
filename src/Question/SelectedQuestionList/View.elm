module Question.SelectedQuestionList.View exposing (..)

import Html exposing (..)
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
import Material.Spinner as Loading
import Material.Toggles as Toggles
import Material.Typography as Typo


-- My modules

import Question.SelectedQuestionList.Types exposing (..)
import Question.Question.Types as Question
import Question.QuestionListGenerate.Types as QuestionListGenerate
import Utils.StringUtils as StringUtils


-- Dialog


dialog : Model -> Html Msg
dialog model =
    let
        generateWithAnswer =
            model.questionListGenerate.generateWithAnswer

        generateWithResolution =
            model.questionListGenerate.generateWithResolution
    in
        Dialog.view
            [ Options.cs "question_dialog"
            ]
            [ Dialog.title []
                [ text "Download"
                ]
            , Dialog.content []
                [ Options.styled p
                    [ Typo.title ]
                    [ text "Opções:" ]
                , Toggles.checkbox Mdl
                    [ 0, 0 ]
                    model.mdl
                    [ Options.onToggle <| QuestionListGenerateMsg QuestionListGenerate.ToggleGenerateWithAnswer
                    , Toggles.ripple
                    , Toggles.value generateWithAnswer
                    ]
                    [ text "Com gabarito" ]
                , Toggles.checkbox Mdl
                    [ 0, 1 ]
                    model.mdl
                    [ Options.onToggle <| QuestionListGenerateMsg QuestionListGenerate.ToggleGenerateWithResolution
                    , Toggles.ripple
                    , Toggles.value generateWithResolution
                    ]
                    [ text "Com resolução" ]
                ]
            , Dialog.actions []
                [ Button.render Mdl
                    [ 0 ]
                    model.mdl
                    [ Dialog.closeOn "click"
                    , Options.onClick <| (QuestionListGenerateMsg <| QuestionListGenerate.QuestionListGenerate model.questionList.questionList)
                    ]
                    [ text "Download" ]
                , Button.render Mdl
                    [ 1 ]
                    model.mdl
                    [ Dialog.closeOn "click" ]
                    [ text "Cancelar" ]
                ]
            ]


view : Model -> Html Msg
view model =
    let
        questionList =
            model.questionList.questionList

        downloading =
            model.questionListGenerate.downloading
    in
        if model.loading then
            Options.div [ Options.cs "question_loader_div" ] [ Options.div [ Options.cs "question_loader" ] [ Loading.spinner [ Loading.active model.loading ] ] ]
        else
            Options.div []
                [ Options.div []
                    [ Options.styled h1
                        [ Typo.display1, Typo.center ]
                        [ text questionList.question_list_header ]
                    , Grid.grid
                        [ Options.cs "questions_list_display"
                        ]
                        (List.map
                            (\question ->
                                Grid.cell
                                    [ size All 3
                                    , Options.css "padding" "8px 8px"
                                    ]
                                    [ questionCardView model question False ]
                            )
                         <|
                            List.map (\q -> q.question) questionList.questions
                        )
                    , Options.div
                        [ Color.background Color.primaryDark
                        , Options.cs "questions_list_action"
                        ]
                        [ Button.render Mdl
                            [ 5, 1 ]
                            model.mdl
                            [ Button.ripple
                            , Button.plain
                            , Color.text Color.white
                            , Options.onClick Dialog
                            , if List.length questionList.questions > 0 && not downloading then
                                Options.nop
                              else
                                Button.disabled
                            ]
                            [ Icon.i "file_download", text "Fazer download" ]
                        , Button.render Mdl
                            [ 5, 2 ]
                            model.mdl
                            [ Button.ripple
                            , Button.plain
                            , Color.text Color.white
                            , Options.onClick QuestionListEdit
                            ]
                            [ Icon.i "mode_edit", text "Editar Lista" ]
                        ]
                    ]
                , Snackbar.view model.snackbar |> Html.map Snackbar
                , dialog model
                ]



-- Question card view


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
            , Options.onClick <| QuestionClick question
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
            , Card.actions [] []
            ]
