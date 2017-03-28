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
import Material.Dialog as Dialog
import Material.Snackbar as Snackbar
import Material.Color as Color
import Utils.MDLUtils as Utils
import Utils.StringUtils as StringUtils


drawerLink : Model -> Html Msg
drawerLink model =
    Layout.navigation
        []
        [ Layout.link
            [ Layout.href "https://goo.gl/forms/NckNklDbJM2uBf3I2"
            ]
            [ Icon.view "add_circle_outline" [ Icon.size18 ], text " Sugerir questão" ]
        , Layout.link
            [ Layout.href "#questions/1"
            ]
            [ Icon.view "view_module" [ Icon.size18 ], text " Selecionar Questões" ]
        , Layout.link
            [ Layout.href "#questions/questionlist/"
            ]
            [ Icon.view "list" [ Icon.size18 ], text " Baixar questões" ]
        , Layout.link
            [ Layout.href "#questions/user_lists/1"
            ]
            [ Icon.view "favorite" [ Icon.size18 ]
            , text " Minhas listas"
            ]
        , Layout.row []
            [ Layout.title
                []
                [ text "Filtros" ]
            ]
        , Card.view
            [ Color.background (Color.color Color.BlueGrey Color.S300)
            , css "width" "192px"
            , css "margin" "0 auto"
            ]
            [ Card.title []
                [ Card.head [ Color.text Color.white ]
                    [ text "Grau de dificuldade" ]
                ]
            , Card.text [ Color.text Color.white ]
                [ div [ class "radio_level" ]
                    [ Toggles.radio Mdl
                        [ 0 ]
                        model.mdl
                        [ Toggles.value (0 == model.filterId)
                        , Toggles.group "QuestionLevel"
                        , Toggles.ripple
                        , Options.onToggle (Filter 0)
                        ]
                        [ text "Todos" ]
                    ]
                , div [ class "radio_level" ]
                    [ Toggles.radio Mdl
                        [ 1 ]
                        model.mdl
                        [ Toggles.value (1 == model.filterId)
                        , Toggles.group "QuestionLevel"
                        , Toggles.ripple
                        , Options.onToggle (Filter 1)
                        ]
                        [ text "Fácil" ]
                    ]
                , div [ class "radio_level" ]
                    [ Toggles.radio Mdl
                        [ 2 ]
                        model.mdl
                        [ Toggles.value (2 == model.filterId)
                        , Toggles.group "QuestionLevel"
                        , Toggles.ripple
                        , Options.onToggle (Filter 2)
                        ]
                        [ text "Médio" ]
                    ]
                , div [ class "radio_level" ]
                    [ Toggles.radio Mdl
                        [ 3 ]
                        model.mdl
                        [ Toggles.value (3 == model.filterId)
                        , Toggles.group "QuestionLevel"
                        , Toggles.ripple
                        , Options.onToggle (Filter 3)
                        ]
                        [ text "Difícil" ]
                    ]
                ]
            ]
        ]



-- Dialog


dialog : Model -> Html Msg
dialog model =
    case model.dialog of
        Delete ->
            Dialog.view
                []
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
                        , Options.onClick QuestionListDelete
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
                []
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
                        , Options.onClick QuestionListClear
                        ]
                        [ text "Confirmar" ]
                    , Button.render Mdl
                        [ 1 ]
                        model.mdl
                        [ Dialog.closeOn "click" ]
                        [ text "Cancelar" ]
                    ]
                ]


view : (Model -> Html Msg) -> Model -> Html Msg
view method model =
    div []
        [ method model
        , Snackbar.view model.snackbar |> Html.map Snackbar
        , dialog model
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


textToChip : String -> Html msg
textToChip s =
    Chip.span
        [ Options.css "margin-right" "5px"
        , Color.background (Color.color Color.Blue Color.S100)
        ]
        [ Chip.content []
            [ text s ]
        ]


cardTitle : Question -> Card.Block msg
cardTitle question =
    Card.title
        [ Color.text Color.white
        , Color.background (Color.color Color.Pink Color.S300)
          -- Clear default padding to encompass scrim
        ]
        [ Options.div
            []
            -- Icon.view "description" [ Icon.size36 ]
            [ Card.head []
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
            , Card.subhead
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
            ]
        ]


viewQuestion : Model -> Html Msg
viewQuestion model =
    let
        question =
            model.question

        questionsId =
            List.map (\q -> q.question.id) model.questionListEdit.questions

        year_text =
            StringUtils.maybeIntToString question.year

        source_text =
            StringUtils.maybeStringToString question.source
    in
        Grid.grid [ Color.background (Color.color Color.Grey Color.S50) ]
            [ Grid.cell
                [ size All 12
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
                        , div [] (List.indexedMap answerView question.answers)
                        , if (List.length question.answers) > 0 then
                            p [] [ correctAnswerView question.answers ]
                          else
                            Options.span [] []
                        , if (question.resolution /= "") then
                            p []
                                [ text "RESOLUÇÃO: "
                                , Markdown.toHtml [] question.resolution
                                ]
                          else
                            Options.span [] []
                        , (List.map textToChip question.tags)
                            |> Options.styled div [ Options.css "margin" "10px 0" ]
                        ]
                    , Card.actions
                        [ Card.border
                        , Color.background (Color.color Color.Blue Color.S600)
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
                          -- , Button.render Mdl
                          --     [ 2, 1, question.id ]
                          --     model.mdl
                          --     [ Button.ripple
                          --     , Button.accent
                          --     , Color.text Color.white
                          --     , css "font-size" "11px"
                          --     , css "width" "33%"
                          --     ]
                          --     [ Icon.view "favorite" [ Icon.size18 ], text " Favoritar" ]
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
                                Options.onClick (QuestionListAdd question)
                            ]
                            (if List.member question.id questionsId then
                                [ text "Adicionada" ]
                             else
                                [ Icon.view "add" [ Icon.size18 ], text " Adicionar" ]
                            )
                        ]
                    ]
                ]
            ]



-- Question card view


questionCardButton : Model -> Bool -> Question -> Card.Block Msg
questionCardButton model add question =
    let
        questionsId =
            List.map (\q -> q.question.id) model.questionListEdit.questions
    in
        if add then
            Card.actions
                [ Card.border
                , Color.background (Color.color Color.Blue Color.S700)
                ]
                [ -- Button.render Mdl
                  --     [ 2, 0, question.id ]
                  --     model.mdl
                  --     [ Button.ripple
                  --     , Button.accent
                  --     , Color.text Color.white
                  --     , css "font-size" "11px"
                  --     , css "width" "50%"
                  --     ]
                  --     [ Icon.view "favorite" [ Icon.size18 ], text " Favoritar" ],
                  Button.render Mdl
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
                        Options.onClick (QuestionListAdd question)
                    ]
                    (if List.member question.id questionsId then
                        [ Icon.i "done", text "Adicionada" ]
                     else
                        [ Icon.view "add" [ Icon.size18 ], text " Adicionar" ]
                    )
                ]
        else
            Card.actions
                [ Card.border
                , Color.background (Color.color Color.Red Color.S700)
                ]
                [ -- Button.render Mdl
                  --     [ 2, 0, question.id ]
                  --     model.mdl
                  --     [ Button.ripple
                  --     , Button.accent
                  --     , Color.text Color.white
                  --     , css "font-size" "11px"
                  --     , css "width" "50%"
                  --     ]
                  --     [ Icon.view "favorite" [ Icon.size18 ], text " Favoritar" ],
                  Button.render Mdl
                    [ 2, 1, question.id ]
                    model.mdl
                    [ Button.ripple
                    , Button.accent
                    , Options.onClick (QuestionListRemove question)
                    , Color.text Color.white
                    , css "font-size" "11px"
                    , css "width" "100%"
                    ]
                    [ Icon.view "remove" [ Icon.size18 ], text " Remover" ]
                ]


questionCardView : Model -> Bool -> Question -> Grid.Cell Msg
questionCardView model add question =
    let
        year_text =
            StringUtils.maybeIntToString question.year

        source_text =
            StringUtils.maybeStringToString question.source
    in
        Grid.cell
            [ size All 3
            , Options.css "padding" "8px 8px"
            ]
            [ Card.view
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
                , (questionCardButton model add question)
                ]
            ]



-- Question Page View


searchTagChip : String -> Html Msg
searchTagChip tag =
    Chip.span
        [ Chip.deleteIcon "cancel"
        , Chip.deleteClick (TagSearchRemove tag)
        , Options.css "margin-right" "5px"
        ]
        [ Chip.content []
            [ text tag ]
        ]


searchView : Model -> Html Msg
searchView model =
    div []
        [ Grid.grid []
            [ Grid.cell [ size All 4 ]
                [ Textfield.render Mdl
                    [ 4, 0 ]
                    model.mdl
                    [ Options.onInput TagSearchInput
                    , Utils.onEnter TagSearchAdd
                    , Textfield.value model.currentTag
                    , Textfield.label "Buscar questões"
                    , Textfield.floatingLabel
                    ]
                    []
                ]
            , Grid.cell [ size All 2 ]
                [ Button.render Mdl
                    [ 4, 1 ]
                    model.mdl
                    [ Button.ripple
                    , Button.colored
                    , Button.raised
                    , Options.onClick TagSearch
                    ]
                    [ text "Buscar" ]
                ]
            , Grid.cell
                [ size All 6 ]
                [ Button.render Mdl
                    [ 4, 2 ]
                    model.mdl
                    [ Button.ripple
                    , Button.colored
                    , Button.flat
                    , Button.link "https://goo.gl/forms/0wUWEPzVn212FTNg1"
                    ]
                    [ text "Não encontrou o que queria? Faça seu pedido!" ]
                ]
            ]
        , Options.styled div
            [ Options.css "margin-left" "5px"
            , Options.css "margin-top" "-25px"
            ]
          <|
            List.map (\tag -> searchTagChip tag) model.tags
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
                            (number + 1)
                            (Options.onClick <| ChangePage (number))
                            (text <| toString number)
                )
            <|
                List.range 1 (ceiling <| (toFloat page.count) / 12)
    in
        Grid.grid [] <|
            (if page.previous /= Nothing then
                [ pageButton model
                    0
                    (Options.onClick <| ChangePage prevPage)
                    (Icon.view "chevron_left" [ Icon.size18 ])
                ]
             else
                []
            )
                ++ numberButtons
                ++ (if page.next /= Nothing then
                        [ pageButton model
                            1
                            (Options.onClick <| ChangePage nextPage)
                            (Icon.view "chevron_right" [ Icon.size18 ])
                        ]
                    else
                        []
                   )


viewQuestionPage : Model -> Html Msg
viewQuestionPage model =
    Options.styled div
        [ Color.background (Color.color Color.Grey Color.S50) ]
        [ Grid.grid []
            [ Grid.cell [ size All 12 ]
                [ searchView model ]
            , Grid.cell [ size All 12 ]
                [ Options.styled p
                    [ Typo.subhead, Options.css "margin" "0 10px" ]
                    [ (if model.questionPage.count == 0 then
                        "Nenhuma questão encontrada"
                       else if model.questionPage.count == 1 then
                        "1 questão encontrada"
                       else
                        toString (model.questionPage.count) ++ " questões encontradas"
                      )
                        |> text
                    ]
                ]
            ]
        , Grid.grid []
            (List.map (questionCardView model True) (List.take 12 model.questionPage.questions))
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
        div
            []
            [ Options.styled h2
                [ Typo.display1, Typo.center ]
                [ text <|
                    if questionList.id == 0 then
                        "Nova lista de questões"
                    else
                        String.concat [ "Editando a lista ", questionList.question_list_header ]
                ]
            , Textfield.render Mdl
                [ 5, 0 ]
                model.mdl
                [ Options.onInput QuestionListHeaderInput
                , Options.css "margin-left" "30px"
                , Textfield.value questionList.question_list_header
                , Textfield.floatingLabel
                , Textfield.label "Digite o Nome da lista"
                ]
                []
            , Options.styled p
                [ Options.css "margin" "0 20px"
                ]
                [ text "Veja abaixo as questões que você selecionou. Para baixá-las, digite um nome para a lista no campo acima, clique em salvar e em seguida Fazer Download." ]
            , Grid.grid [ Options.cs "questions_list_display" ]
                (List.map (questionCardView model False) <| List.map (\q -> q.question) questionList.questions)
            , if questionList.id == 0 then
                viewQuestionListButtonNew model
              else
                viewQuestionListButtonEdit model
            ]


viewQuestionListButtonNew : Model -> Html Msg
viewQuestionListButtonNew model =
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
            , Options.onClick QuestionListSave
            ]
            [ Icon.i "save", text "Salvar" ]
        , Button.render Mdl
            [ 5, 2 ]
            model.mdl
            [ Button.ripple
            , Button.plain
            , Color.text Color.white
            , Dialog.openOn "click"
            , Options.onClick (Dialog Clear)
            , if List.length model.questionListEdit.questions > 0 then
                Options.nop
              else
                Button.disabled
              -- , Options.onClick QuestionListClear
            ]
            [ Icon.i "delete_forever", text "Limpar lista" ]
        ]


viewQuestionListButtonEdit : Model -> Html Msg
viewQuestionListButtonEdit model =
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
            , Options.onClick QuestionListSave
            ]
            [ Icon.i "save", text "Salvar" ]
        , Button.render Mdl
            [ 5, 2 ]
            model.mdl
            [ Button.ripple
            , Button.plain
            , Color.text Color.white
            , Dialog.openOn "click"
            , Options.onClick (Dialog Clear)
            , if List.length model.questionListEdit.questions > 0 then
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
            , Dialog.openOn "click"
            , Options.onClick (Dialog Delete)
              -- , Options.onClick QuestionListDelete
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
            , Grid.grid
                [ Options.cs "questions_list_display"
                ]
                (List.map (questionCardView model False) <| List.map (\q -> q.question) questionList.questions)
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
                    , Options.onClick <| QuestionListGenerate questionList
                    , if List.length questionList.questions > 0 then
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
                    , Options.onClick <| QuestionListEdit questionList
                    ]
                    [ Icon.i "mode_edit", text "Editar Lista" ]
                ]
            ]
