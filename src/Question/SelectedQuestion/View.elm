module Question.SelectedQuestion.View exposing (..)

import Html exposing (..)
import Markdown
import Material.Button as Button
import Material.Card as Card
import Material.Chip as Chip
import Material.Color as Color
import Material.Grid as Grid
import Material.Grid exposing (grid, cell, size, offset, Device(..))
import Material.Icon as Icon
import Material.Layout as Layout
import Material.Options as Options exposing (css)
import Material.Typography as Typo


-- My modules

import Question.SelectedQuestion.Types exposing (..)
import Question.Question.Types as Question
import Question.QuestionList.Types as QuestionList
import Utils.StringUtils as StringUtils


view : Model -> Html Msg
view model =
    let
        question =
            model.question.question

        questionsId =
            List.map (\q -> q.question.id) model.questionList.questionList.questions

        year_text =
            StringUtils.maybeIntToString question.year

        source_text =
            StringUtils.maybeStringToString question.source

        resolution =
            StringUtils.maybeStringToString question.resolution

        related_questions =
            case question.related_questions of
                Question.RelatedQuestion l ->
                    l

        question_parent =
            case question.question_parent of
                Question.QuestionParent p ->
                    p
    in
        Grid.grid [ Color.background (Color.color Color.Grey Color.S50) ]
            [ Grid.cell
                [ size All
                    (if List.length related_questions > 0 then
                        9
                     else
                        12
                    )
                , Options.css "padding" "8px 8px"
                ]
                [ Card.view
                    [ css "width" "100%"
                    , Options.cs "mdl-shadow--2dp"
                    ]
                    [ cardTitle question
                    , Card.text
                        [ css "min-height" "196px"
                        , Options.cs "question_card"
                        ]
                        [ case question_parent of
                            Just p ->
                                Markdown.toHtml [] p.question_statement

                            Nothing ->
                                span [] []
                        , (if (String.length year_text > 0) && (String.length source_text > 0) then
                            text ("(" ++ year_text ++ " - " ++ source_text ++ ")")
                           else if String.length year_text > 0 then
                            text ("(" ++ year_text ++ ")")
                           else if String.length source_text > 0 then
                            text ("(" ++ source_text ++ ")")
                           else
                            text ""
                          )
                        , Markdown.toHtml [] question.question_statement
                        , div [] (List.indexedMap answerView question.answers)
                        , if (List.length question.answers) > 0 then
                            p [] [ correctAnswerView question.answers ]
                          else
                            Options.span [] []
                        , if (resolution /= "") then
                            p []
                                [ text "RESOLUÇÃO: "
                                , Markdown.toHtml [] resolution
                                ]
                          else
                            Options.span [] []
                        , (List.map textToChip question.tags)
                            |> Options.styled div [ Options.css "margin" "10px 0" ]
                        , if List.length question.question_lists > 0 then
                            [ Options.styled p
                                [ Typo.title ]
                                [ text "Lista(s) que esta questão aparece:"
                                ]
                            ]
                                ++ (List.map questionListToLink question.question_lists)
                                |> Options.styled div [ Options.css "margin" "10px 0" ]
                          else
                            Options.span [] []
                        ]
                    , Card.actions
                        [ Card.border
                        , Color.background (Color.color Color.Green Color.S600)
                        , Options.css "height" "52px"
                        ]
                        [ Button.render Mdl
                            [ 2, 0, question.id ]
                            model.mdl
                            [ Button.ripple
                            , Button.accent
                            , Color.text Color.white
                            , css "font-size" "11px"
                            , css "width" "50%"
                            , Options.onClick QuestionBack
                            ]
                            [ Icon.view "arrow_back" [ Icon.size18 ], text " Voltar" ]
                        , Button.render Mdl
                            [ 2, 2, question.id ]
                            model.mdl
                            [ Button.ripple
                            , Button.accent
                            , Color.text Color.white
                            , css "font-size" "11px"
                            , css "width" "50%"
                            , if List.member question.id questionsId then
                                Button.disabled
                              else
                                Options.onClick <| QuestionListMsg (QuestionList.QuestionListAdd question)
                            ]
                            (if List.member question.id questionsId then
                                [ text "Adicionada" ]
                             else
                                [ Icon.view "add" [ Icon.size18 ], text " Adicionar" ]
                            )
                        ]
                    ]
                ]
            , if List.length related_questions > 0 then
                Grid.cell
                    [ size All 3 ]
                <|
                    Options.styled p
                        [ Typo.title ]
                        [ text "Questões relacionadas"
                        ]
                        :: (List.map
                                (\question ->
                                    Options.div [ Options.css "margin-bottom" "20px" ]
                                        [ questionCardView model NoneQuestionButton question ]
                                )
                                related_questions
                           )
              else
                Grid.cell
                    [ size All 0 ]
                    []
            ]



-- Question View


answerView : Int -> Question.Answer -> Html Msg
answerView index answer =
    Markdown.toHtml [] <| String.concat [ StringUtils.intToResponseString index, ") ", StringUtils.removeEnters answer.answer_text ]


correctAnswerView : List Question.Answer -> Html Msg
correctAnswerView answers =
    let
        res =
            List.indexedMap
                (\index answer ->
                    if answer.is_correct then
                        Just (StringUtils.intToResponseString index)
                    else
                        Nothing
                )
                answers

        letter =
            (List.foldr
                (\a b ->
                    case a of
                        Just t ->
                            t

                        Nothing ->
                            b
                )
                ""
                res
            )
    in
        text <| String.toUpper <| String.concat [ "RESPOSTA: ", letter ]


textToChip : String -> Html msg
textToChip s =
    Chip.span
        [ Options.css "margin-right" "5px"
        , Color.background (Color.color Color.Blue Color.S100)
        ]
        [ Chip.content []
            [ text s ]
        ]


questionListToLink : Question.QuestionListInfo -> Html msg
questionListToLink questionListInfo =
    let
        owner =
            questionListInfo.owner
    in
        Options.div []
            [ Options.styled a
                [ Typo.subhead, Layout.href ("#questions/questionlists/" ++ toString questionListInfo.id) ]
                [ text questionListInfo.question_list_header ]
            , text " criada por "
            , Options.styled a
                [ Typo.subhead, Layout.href ("#users/" ++ toString owner.id) ]
                [ text owner.username ]
            ]


cardTitle : Question.Question -> Card.Block msg
cardTitle question =
    Card.title
        [ -- Color.text Color.white
          -- , Color.background (Color.color Color.BlueGrey Color.S100)
          Options.css "height" "80px"
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


questionCardButton : Model -> QuestionButtonType -> Question.Question -> Card.Block Msg
questionCardButton model questionButtonType question =
    let
        questionsId =
            List.map (\q -> q.question.id) model.questionList.questionList.questions
    in
        case questionButtonType of
            AddQuestionButton ->
                Card.actions
                    [ Card.border
                    , Color.background (Color.color Color.Green Color.S600)
                    , Options.css "height" "52px"
                    ]
                    [ Button.render Mdl
                        [ 2, 1, question.id ]
                        model.mdl
                        [ Button.ripple
                        , Button.accent
                        , Color.text Color.white
                        , css "font-size" "11px"
                        , css "width" "100%"
                        , if List.member question.id questionsId then
                            Button.disabled
                          else
                            Options.onClick <| QuestionListMsg (QuestionList.QuestionListAdd question)
                        ]
                        (if List.member question.id questionsId then
                            [ Icon.i "done", text "Adicionada" ]
                         else
                            [ Icon.view "add" [ Icon.size18 ], text " Adicionar" ]
                        )
                    ]

            RemoveQuestionButton ->
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

            NoneQuestionButton ->
                Card.actions [] []


questionCardView : Model -> QuestionButtonType -> Question.Question -> Html Msg
questionCardView model questionButtonType question =
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
            , Options.onClick <| QuestionMsg (Question.GetQuestion question.id)
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
            , (questionCardButton model questionButtonType question)
            ]
