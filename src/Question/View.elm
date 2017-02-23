module Question.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (id, type_, for, value, class, href)
import Html.Events exposing (..)
import Question.Types exposing (..)
import Markdown
import Material.Textfield as Textfield
import Material.Button as Button
import Material.Grid as Grid
import Material.Card as Card
import Material.Chip as Chip
import Material.Icon as Icon
import Material.Layout as Layout
import Material.Toggles as Toggles
import Material.Options as Options exposing (css)
import Material.Grid exposing (grid, cell, size, offset, Device(..))
import Material.Typography as Typo
import Material.Color as Color
import Utils.MDLUtils as Utils


drawerLink : Model -> Html Msg
drawerLink model =
    Layout.navigation
        []
        [ Layout.link
            [ Layout.href "#questions/1"
            ]
            [ text "Ver Questões" ]
        , Layout.link
            [ Layout.href "#questions/questionlist/"
            ]
            [ Icon.view "view_module" [ Icon.size18 ], text " Lista de questão atual" ]
        , Layout.row []
            [ Layout.title
                []
                [ text "Filtros" ]
            ]
        , text "nível: "
        , div
            []
            [ Toggles.radio Mdl
                [ 0 ]
                model.mdl
                [ Toggles.value (0 == model.filterId)
                , Toggles.group "MyRadioGroup"
                , Toggles.ripple
                , Options.onToggle (Filter 0)
                ]
                [ text "Todos" ]
            , Toggles.radio Mdl
                [ 1 ]
                model.mdl
                [ Toggles.value (1 == model.filterId)
                , Toggles.group "MyRadioGroup"
                , Toggles.ripple
                , Options.onToggle (Filter 1)
                ]
                [ text "Fácil" ]
            , Toggles.radio Mdl
                [ 2 ]
                model.mdl
                [ Toggles.value (2 == model.filterId)
                , Toggles.group "MyRadioGroup"
                , Toggles.ripple
                , Options.onToggle (Filter 2)
                ]
                [ text "Médio" ]
            , Toggles.radio Mdl
                [ 3 ]
                model.mdl
                [ Toggles.value (3 == model.filterId)
                , Toggles.group "MyRadioGroup"
                , Toggles.ripple
                , Options.onToggle (Filter 3)
                ]
                [ text "Difícil" ]
            ]
        ]


view : Model -> Html Msg
view model =
    let
        question =
            model.question
    in
        Options.div []
            [ questionView question
            , text (toString model)
            , text model.error
            ]



-- Question View


questionView : Question -> Html Msg
questionView question =
    Options.div []
        [ Html.h1 [] [ text "Question" ]
        , p [] [ text (toString question.id) ]
        , p [] [ text question.question_header ]
        , p [] [ Markdown.toHtml [] question.question_text ]
        , p []
            [ text
                (case question.level of
                    Just level ->
                        level

                    Nothing ->
                        ""
                )
            ]
        , p [] [ text (toString question.credit_cost) ]
        , p [] [ text (toString question.tags) ]
        , p [] [ text (toString question.answers) ]
        ]



-- Question Page View


questionCardView : Model -> Question -> Grid.Cell Msg
questionCardView model question =
    Grid.cell
        [ size All 3
        , Options.css "padding" "8px 8px"
        ]
        [ Card.view
            [ Color.background (Color.color Color.LightGreen Color.S500)
            , css "width" "100%"
            ]
            [ Card.title
                [ Color.text Color.white
                , css "padding" "16px"
                , css "display" "flex"
                , css "align-items" "center"
                , css "justify-content" "center"
                , Card.border
                  -- Clear default padding to encompass scrim
                ]
                [ Icon.view "description" [ Icon.size36 ]
                , text "Português"
                ]
            , Card.text
                [ css "height" "196px"
                ]
                [ Markdown.toHtml [] (String.slice 0 100 question.question_header) ]
            , Card.actions
                [ Card.border
                , Color.background (Color.color Color.LightGreen Color.S900)
                ]
                [ Button.render Mdl
                    [ 2, 0, question.id ]
                    model.mdl
                    [ Button.ripple
                    , Button.accent
                    , Color.text Color.white
                    , css "font-size" "11px"
                    , css "width" "50%"
                    ]
                    [ Icon.view "favorite" [ Icon.size18 ], text " Favoritar" ]
                , Button.render Mdl
                    [ 2, 1, question.id ]
                    model.mdl
                    [ Button.ripple
                    , Button.accent
                    , Color.text Color.white
                    , css "font-size" "11px"
                    , css "width" "50%"
                    , if List.member question model.questionList.questions then
                        Button.disabled
                      else
                        Options.onClick (QuestionListAdd question)
                    ]
                    (if List.member question model.questionList.questions then
                        [ text "Adicionado" ]
                     else
                        [ Icon.view "add" [ Icon.size18 ], text " Adicionar" ]
                    )
                ]
            ]
        ]


searchTagChip : String -> Html Msg
searchTagChip tag =
    Chip.span
        [ Chip.deleteIcon "cancel"
        , Chip.deleteClick (TagSearchRemove tag)
        ]
        [ Chip.content []
            [ text tag ]
        ]


searchView : Model -> Html Msg
searchView model =
    Options.div []
        [ Textfield.render Mdl
            [ 4, 0 ]
            model.mdl
            [ Options.onInput TagSearchInput
            , Utils.onEnter TagSearchAdd
            , Textfield.value model.currentTag
            , Textfield.label "Search"
            ]
            []
        , Grid.grid [] <|
            List.map (\tag -> Grid.cell [ size All 2 ] [ searchTagChip tag ]) model.tags
        , Button.render Mdl
            [ 4, 1 ]
            model.mdl
            [ Button.ripple
            , Button.colored
            , Button.raised
            , Options.onClick TagSearch
            ]
            [ text "Search" ]
        ]


pageButton : Model -> Int -> Button.Property Msg -> Html Msg -> Grid.Cell Msg
pageButton model id property text =
    Grid.cell [ size All 1 ]
        [ Button.render Mdl
            [ 3, id ]
            model.mdl
            [ Button.ripple
            , Button.colored
            , Button.raised
            , property
            ]
            [ text ]
        ]


questionPageControls : Model -> Html Msg
questionPageControls model =
    let
        page =
            model.questionPage

        prevPage =
            page.actual - 1

        nextPage =
            page.actual + 1

        numberButtons =
            List.map
                (\number ->
                    if number == page.actual then
                        pageButton model number Button.disabled (text <| toString page.actual)
                    else
                        pageButton model
                            number
                            (Options.onClick <| ChangePage (number))
                            (text <| toString number)
                )
            <|
                List.range 1 (ceiling <| (toFloat page.count) / 9)
    in
        Grid.grid [] <|
            (if page.previous /= Nothing then
                [ pageButton model
                    prevPage
                    (Options.onClick <| ChangePage prevPage)
                    (Icon.view "chevron_left" [ Icon.size18 ])
                ]
             else
                []
            )
                ++ numberButtons
                ++ (if page.next /= Nothing then
                        [ pageButton model
                            nextPage
                            (Options.onClick <| ChangePage nextPage)
                            (Icon.view "chevron_right" [ Icon.size18 ])
                        ]
                    else
                        []
                   )


viewQuestionPage : Model -> Html Msg
viewQuestionPage model =
    div []
        [ Grid.grid []
            [ Grid.cell [ size All 12 ]
                [ searchView model ]
            ]
        , Grid.grid []
            (List.map (questionCardView model) (List.take 9 model.questionPage.questions))
        , questionPageControls model
        , text model.error
        ]



-- Question List View


questionListCardView : Model -> Question -> Grid.Cell Msg
questionListCardView model question =
    Grid.cell
        [ size All 3
        , Options.css "padding" "8px 8px"
        ]
        [ Card.view
            [ Color.background (Color.color Color.LightGreen Color.S500)
            , css "width" "100%"
            ]
            [ Card.title
                [ Color.text Color.white
                , css "padding" "8px"
                , css "display" "flex"
                , css "align-items" "center"
                , css "justify-content" "center"
                , Card.border
                  -- Clear default padding to encompass scrim
                ]
                [ Icon.view "description" [ Icon.size36 ]
                , text "Português"
                ]
            , Card.text
                [ css "height" "196px"
                ]
                [ Markdown.toHtml [] (String.slice 0 100 question.question_header) ]
            , Card.actions
                [ Card.border
                , Color.background (Color.color Color.LightGreen Color.S900)
                ]
                [ Button.render Mdl
                    [ 6, 0, question.id ]
                    model.mdl
                    [ Button.ripple
                    , Button.accent
                    , Color.text Color.white
                    , css "font-size" "11px"
                    , css "width" "50%"
                    ]
                    [ Icon.view "favorite" [ Icon.size18 ], text " Favoritar" ]
                , Button.render Mdl
                    [ 6, 1, question.id ]
                    model.mdl
                    [ Button.ripple
                    , Button.accent
                    , Options.onClick (QuestionListRemove question)
                    , Color.text Color.white
                    , css "font-size" "11px"
                    , css "width" "50%"
                    ]
                    [ Icon.view "remove" [ Icon.size18 ], text " Remover" ]
                ]
            ]
        ]


viewQuestionList : Model -> Html Msg
viewQuestionList model =
    div []
        [ Options.styled h1
            [ Typo.display1, Typo.center ]
            [ text "Lista de questões" ]
        , Textfield.render Mdl
            [ 5, 0 ]
            model.mdl
            [ Options.onInput QuestionListHeaderInput
            , Textfield.value model.questionList.question_list_header
            , Textfield.floatingLabel
            , Textfield.label "Nome da lista"
            ]
            []
        , Grid.grid []
            (List.map (questionListCardView model) model.questionList.questions)
        , Button.render Mdl
            [ 5, 1 ]
            model.mdl
            [ Button.ripple
            , Button.colored
            , Button.raised
            , Options.onClick QuestionListGenerate
            ]
            [ text "Gerar Lista" ]
        , Button.render Mdl
            [ 5, 2 ]
            model.mdl
            [ Button.ripple
            , Button.colored
            , Button.raised
            , Options.onClick QuestionListSave
            ]
            [ text "Salvar" ]
        , Button.render Mdl
            [ 5, 3 ]
            model.mdl
            [ Button.ripple
            , Button.colored
            , Button.raised
            , Options.onClick QuestionListDelete
            ]
            [ text "Apagar Lista" ]
        ]
