module Question.View exposing (..)

import Html exposing (..)
import Date
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
import Material.List as Lists
import Material.Layout as Layout
import Material.Toggles as Toggles
import Material.Options as Options exposing (css)
import Material.Grid exposing (grid, cell, size, offset, Device(..))
import Material.Typography as Typo
import Material.Snackbar as Snackbar
import Material.Color as Color
import Utils.MDLUtils as Utils
import Utils.StringUtils as StringUtils


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
        , Layout.link
            [ Layout.href "#questions/minequestionlists/1"
            ]
            [ text "Minhas listas" ]
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


view : (Model -> Html Msg) -> Model -> Html Msg
view method model =
    div []
        [ method model
        , Snackbar.view model.snackbar |> Html.map Snackbar
        ]



-- Question View


answerView : Int -> Answer -> Html Msg
answerView index answer =
    Markdown.toHtml [] <| String.concat [ StringUtils.intToResponseString index, ") ", StringUtils.removeEnters answer.answer_text ]


correctAnswerView : List Answer -> Html Msg
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


viewQuestion : Model -> Html Msg
viewQuestion model =
    let
        question =
            model.question

        questions =
            List.map (\q -> q.question) model.questionListEdit.questions
    in
        Grid.grid []
            [ Grid.cell
                [ size All 12
                , Options.css "padding" "8px 8px"
                ]
                [ Card.view
                    [ Color.background (Color.color Color.LightGreen Color.S500)
                    , css "width" "100%"
                    , Options.onClick <| QuestionClick question
                    ]
                    [ Card.title
                        [ Color.text Color.white
                        , css "padding" "16px"
                        , css "display" "flex"
                        , Card.border
                        ]
                        [ Icon.view "description" [ Icon.size36 ]
                        , text "Português"
                        ]
                    , Card.text
                        [ css "min-height" "196px"
                        ]
                        [ Markdown.toHtml [] question.question_header
                        , Markdown.toHtml [] question.question_text
                        , div [] (List.indexedMap answerView question.answers)
                        , correctAnswerView question.answers
                        ]
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
                            , css "width" "33%"
                            , Options.onClick QuestionBack
                            ]
                            [ Icon.view "arrow_back" [ Icon.size18 ], text " Voltar" ]
                        , Button.render Mdl
                            [ 2, 1, question.id ]
                            model.mdl
                            [ Button.ripple
                            , Button.accent
                            , Color.text Color.white
                            , css "font-size" "11px"
                            , css "width" "33%"
                            ]
                            [ Icon.view "favorite" [ Icon.size18 ], text " Favoritar" ]
                        , Button.render Mdl
                            [ 2, 2, question.id ]
                            model.mdl
                            [ Button.ripple
                            , Button.accent
                            , Color.text Color.white
                            , css "font-size" "11px"
                            , css "width" "33%"
                            , if List.member question questions then
                                Button.disabled
                              else
                                Options.onClick (QuestionListAdd question)
                            ]
                            (if List.member question questions then
                                [ text "Adicionado" ]
                             else
                                [ Icon.view "add" [ Icon.size18 ], text " Adicionar" ]
                            )
                        ]
                    ]
                ]
            ]



-- Question Page View


questionCardButton : Model -> Bool -> Question -> Card.Block Msg
questionCardButton model add question =
    let
        questions =
            List.map (\q -> q.question) model.questionListEdit.questions
    in
        if add then
            Card.actions
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
                    , if List.member question questions then
                        Button.disabled
                      else
                        Options.onClick (QuestionListAdd question)
                    ]
                    (if List.member question questions then
                        [ text "Adicionado" ]
                     else
                        [ Icon.view "add" [ Icon.size18 ], text " Adicionar" ]
                    )
                ]
        else
            Card.actions
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
                    , Options.onClick (QuestionListRemove question)
                    , Color.text Color.white
                    , css "font-size" "11px"
                    , css "width" "50%"
                    ]
                    [ Icon.view "remove" [ Icon.size18 ], text " Remover" ]
                ]


questionCardView : Model -> Bool -> Question -> Grid.Cell Msg
questionCardView model add question =
    Grid.cell
        [ size All 3
        , Options.css "padding" "8px 8px"
        ]
        [ Card.view
            [ Color.background (Color.color Color.LightGreen Color.S500)
            , css "width" "100%"
            , Options.onClick <| QuestionClick question
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
            , (questionCardButton model add question)
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
            (List.map (questionCardView model True) (List.take 9 model.questionPage.questions))
        , questionPageControls model
        , text model.error
        ]



-- Question List View


viewQuestionList : Model -> Html Msg
viewQuestionList model =
    let
        questionList =
            model.questionListEdit
    in
        div []
            [ Options.styled h1
                [ Typo.display1, Typo.center ]
                [ text "Lista de questões" ]
            , Textfield.render Mdl
                [ 5, 0 ]
                model.mdl
                [ Options.onInput QuestionListHeaderInput
                , Textfield.value questionList.question_list_header
                , Textfield.floatingLabel
                , Textfield.label "Nome da lista"
                ]
                []
            , Grid.grid []
                (List.map (questionCardView model False) <| List.map (\q -> q.question) questionList.questions)
            , if questionList.id == 0 then
                viewQuestionListButtonNew model
              else
                viewQuestionListButtonEdit model
            ]


viewQuestionListButtonNew : Model -> Html Msg
viewQuestionListButtonNew model =
    div []
        [ Button.render Mdl
            [ 5, 1 ]
            model.mdl
            [ Button.ripple
            , Button.colored
            , Button.raised
            , Options.onClick QuestionListSave
            ]
            [ text "Salvar" ]
        , Button.render Mdl
            [ 5, 2 ]
            model.mdl
            [ Button.ripple
            , Button.colored
            , Button.raised
            , Options.onClick QuestionListClear
            ]
            [ text "Limpar lista" ]
        ]


viewQuestionListButtonEdit : Model -> Html Msg
viewQuestionListButtonEdit model =
    div []
        [ Button.render Mdl
            [ 5, 1 ]
            model.mdl
            [ Button.ripple
            , Button.colored
            , Button.raised
            , Options.onClick QuestionListSave
            ]
            [ text "Salvar" ]
        , Button.render Mdl
            [ 5, 2 ]
            model.mdl
            [ Button.ripple
            , Button.colored
            , Button.raised
            , Options.onClick QuestionListClear
            ]
            [ text "Limpar lista" ]
        , Button.render Mdl
            [ 5, 3 ]
            model.mdl
            [ Button.ripple
            , Button.colored
            , Button.raised
            , Options.onClick QuestionListDelete
            ]
            [ text "Apagar lista" ]
        ]



-- Mine Question List Page


questionListItems : Model -> QuestionList -> Html Msg
questionListItems model questionList =
    let
        createdDate =
            (Date.fromString questionList.create_date)
    in
        Lists.li [ Lists.withSubtitle ]
            -- ! Required on every Lists.li containing subtitle.
            [ Lists.content
                [ Options.attribute <| Html.Events.onClick (QuestionListClick questionList.id)
                , Options.css "cursor" "pointer"
                , Options.css "cursor" "hand"
                ]
                [ text questionList.question_list_header
                , Lists.subtitle [] [ text <| String.concat [ "Lista criada em: ", StringUtils.dateToString createdDate, " às ", StringUtils.timeToString createdDate ] ]
                ]
              -- , Lists.content2 []
              --     [ Toggles.checkbox Mdl
              --         [ 4 ]
              --         model.mdl
              --         [ Toggles.value 4
              --            , Options.onToggle SelectQuestionList
              --         ]
              --         []
              --     ]
            ]


viewQuestionListPage : Model -> Html Msg
viewQuestionListPage model =
    div []
        [ Lists.ul [] <| List.map (questionListItems model) model.questionListPage.questionLists
        ]


viewSelectedQuestionList : Model -> Html Msg
viewSelectedQuestionList model =
    let
        questionList =
            model.questionListSelected
    in
        div []
            [ Options.styled h1
                [ Typo.display1, Typo.center ]
                [ text questionList.question_list_header ]
            , Grid.grid []
                (List.map (questionCardView model False) <| List.map (\q -> q.question) questionList.questions)
            , Button.render Mdl
                [ 5, 1 ]
                model.mdl
                [ Button.ripple
                , Button.colored
                , Button.raised
                , Options.onClick <| QuestionListGenerate questionList
                ]
                [ text "Gerar Lista" ]
            , Button.render Mdl
                [ 5, 2 ]
                model.mdl
                [ Button.ripple
                , Button.colored
                , Button.raised
                , Options.onClick <| QuestionListEdit questionList
                ]
                [ text "Editar Lista" ]
            ]
