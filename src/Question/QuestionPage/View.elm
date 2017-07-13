module Question.QuestionPage.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, placeholder, autofocus, value, name, id)
import Html.Events exposing (..)
import Json.Decode as Json
import Markdown
import Json.Decode as Json
import Material.Badge as Badge
import Material.Button as Button
import Material.Card as Card
import Material.Chip as Chip
import Material.Color as Color
import Material.Grid as Grid
import Material.Grid exposing (grid, cell, size, offset, Device(..))
import Material.Icon as Icon
import Material.Layout as Layout
import Material.Spinner as Loading
import Material.Options as Options exposing (css)
import Material.Snackbar as Snackbar
import Material.Toggles as Toggles
import Material.Typography as Typo


-- My modules

import Question.QuestionPage.Types exposing (..)
import Question.Question.Types as Question
import Question.QuestionList.Types as QuestionList
import Utils.StringUtils as StringUtils


onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Json.succeed msg
            else
                Json.fail "not ENTER"
    in
        on "keydown" (Json.andThen isEnter keyCode)


filters : Model -> List (Html Msg)
filters model =
    let
        levelFilters =
            model.filters.levelFilters

        subjectFilters =
            model.filters.subjectFilters

        educationLevelFilters =
            model.filters.educationLevelFilters

        filters =
            model.filters

        subjects =
            model.subjects
    in
        [ Layout.row []
            [ Layout.title
                []
                [ Icon.i "filter_list", text " Filtros" ]
            ]
        , Card.view
            [ Color.background (Color.color Color.Grey Color.S200)
            , Options.cs "drawer-nav-link"
            ]
            [ Card.title [ Options.onClick <| ToggleFilter SubjectToggle ]
                [ Card.head
                    []
                    [ Icon.i <|
                        if filters.subjectToggle then
                            "remove"
                        else
                            "add"
                    , text "Disciplinas"
                    ]
                ]
            , Card.text
                [ if filters.subjectToggle then
                    Options.cs "show-filter"
                  else
                    Options.cs "hide-filter"
                ]
                ([ Options.div [ Options.cs "radio_level" ]
                    [ Toggles.checkbox Mdl
                        [ 9, 1 ]
                        model.mdl
                        [ Toggles.value (subjectFilters == [])
                        , Toggles.group "FilterSubject"
                        , Toggles.ripple
                        , Options.onToggle (FilterSubject AllSubject)
                        , Options.cs "question_radio_span"
                        ]
                        [ text "Todos" ]
                    ]
                 ]
                    ++ List.indexedMap
                        (\index subject ->
                            Options.div [ Options.cs "radio_level" ]
                                [ Toggles.checkbox Mdl
                                    [ 9, index + 2 ]
                                    model.mdl
                                    [ Toggles.value (List.member subject.slug subjectFilters)
                                    , Toggles.group "FilterSubject"
                                    , Toggles.ripple
                                    , Options.onToggle (FilterSubject (StringSubject subject.slug))
                                    , Options.cs "question_radio_span"
                                    ]
                                    [ text subject.subject_name ]
                                ]
                        )
                        subjects
                )
            ]
        , Card.view
            [ Color.background (Color.color Color.Grey Color.S200)
            , Options.cs "drawer-nav-link"
            ]
            [ Card.title [ Options.onClick <| ToggleFilter LevelToggle ]
                [ Card.head
                    []
                    [ Icon.i <|
                        if filters.levelToggle then
                            "remove"
                        else
                            "add"
                    , text "Grau de dificuldade"
                    ]
                ]
            , Card.text
                [ if filters.levelToggle then
                    Options.cs "show-filter"
                  else
                    Options.cs "hide-filter"
                ]
                [ Options.div [ Options.cs "radio_level" ]
                    [ Toggles.checkbox Mdl
                        [ 8, 0 ]
                        model.mdl
                        [ Toggles.value (levelFilters == [])
                        , Toggles.group "FilterLevel"
                        , Toggles.ripple
                        , Options.onToggle (FilterLevel AllLevel)
                        , Options.cs "question_radio_span"
                        ]
                        [ text "Todos" ]
                    ]
                , Options.div [ Options.cs "radio_level" ]
                    [ Toggles.checkbox Mdl
                        [ 8, 1 ]
                        model.mdl
                        [ Toggles.value (List.member EasyLevel levelFilters)
                        , Toggles.group "FilterLevel"
                        , Toggles.ripple
                        , Options.onToggle (FilterLevel EasyLevel)
                        , Options.cs "question_radio_span"
                        ]
                        [ text "Fácil" ]
                    ]
                , Options.div [ Options.cs "radio_level" ]
                    [ Toggles.checkbox Mdl
                        [ 8, 2 ]
                        model.mdl
                        [ Toggles.value (List.member MediumLevel levelFilters)
                        , Toggles.group "FilterLevel"
                        , Toggles.ripple
                        , Options.onToggle (FilterLevel MediumLevel)
                        , Options.cs "question_radio_span"
                        ]
                        [ text "Médio" ]
                    ]
                , Options.div [ Options.cs "radio_level" ]
                    [ Toggles.checkbox Mdl
                        [ 8, 3 ]
                        model.mdl
                        [ Toggles.value (List.member HardLevel levelFilters)
                        , Toggles.group "FilterLevel"
                        , Toggles.ripple
                        , Options.onToggle (FilterLevel HardLevel)
                        , Options.cs "question_radio_span"
                        ]
                        [ text "Difícil" ]
                    ]
                ]
            ]
        , Card.view
            [ Color.background (Color.color Color.Grey Color.S200)
            , Options.cs "drawer-nav-link"
            ]
            [ Card.title [ Options.onClick <| ToggleFilter EducationToggle ]
                [ Card.head
                    []
                    [ Icon.i <|
                        if filters.educationToggle then
                            "remove"
                        else
                            "add"
                    , text "Nível de ensino"
                    ]
                ]
            , Card.text
                [ if filters.educationToggle then
                    Options.cs "show-filter"
                  else
                    Options.cs "hide-filter"
                ]
                ([ Options.div [ Options.cs "radio_level" ]
                    [ Toggles.checkbox Mdl
                        [ 10, 1 ]
                        model.mdl
                        [ Toggles.value (educationLevelFilters == [])
                        , Toggles.group "FilterEducationLevel"
                        , Toggles.ripple
                        , Options.onToggle (FilterEducationLevel AllEducationLevel)
                        , Options.cs "question_radio_span"
                        ]
                        [ text "Todos" ]
                    ]
                 ]
                    ++ List.indexedMap
                        (\index educationLevel ->
                            Options.div [ Options.cs "radio_level" ]
                                [ Toggles.checkbox Mdl
                                    [ 10, index + 2 ]
                                    model.mdl
                                    [ Toggles.value (List.member educationLevel educationLevelFilters)
                                    , Toggles.group "FilterEducationLevel"
                                    , Toggles.ripple
                                    , Options.onToggle (FilterEducationLevel (StringEducationLevel educationLevel))
                                    , Options.cs "question_radio_span"
                                    ]
                                    [ text educationLevel ]
                                ]
                        )
                        [ "Ensino Médio", "Fundamental II - anos finais", "Fundamental II - anos iniciais", "Ensino Superior" ]
                )
            ]
        ]


view : Model -> Html Msg
view model =
    if model.loading then
        Options.div [ Options.cs "question_loader_div" ] [ Options.div [ Options.cs "question_loader" ] [ Loading.spinner [ Loading.active model.loading ] ] ]
    else
        Options.div []
            [ Options.styled div
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
                    (List.map
                        (\question ->
                            Grid.cell
                                [ size All 3
                                , Options.css "padding" "8px 8px"
                                ]
                                [ questionCardView model question False ]
                        )
                        (List.take 12 model.questionPage.questions)
                    )
                , questionPageControls model
                , Button.render Mdl
                    [ 10 ]
                    model.mdl
                    [ Options.cs "question_selected_button"
                    , (if List.length model.questionList.questionList.questions > 0 then
                        Badge.add <| toString <| List.length model.questionList.questionList.questions
                       else
                        Options.nop
                      )
                    , Badge.overlap
                    , Color.text Color.white
                    , Button.ripple
                    , Button.colored
                    , Button.raised
                    , Options.onClick SelectedQuestions
                    ]
                    [ text "Questões selecionadas" ]
                ]
            , Snackbar.view model.snackbar |> Html.map Snackbar
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


questionCardButton : Model -> Question.Question -> Card.Block Msg
questionCardButton model question =
    let
        questionsId =
            List.map (\q -> q.question.id) model.questionList.questionList.questions
    in
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
            , (questionCardButton model question)
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
    Options.div []
        [ Grid.grid []
            [ Grid.cell [ size All 11 ]
                [ input
                    [ class "search-input"
                    , placeholder "Digite o(s) termo(s) e encontre questões relacionadas"
                    , value model.filters.currentTag
                    , onInput TagSearchInput
                    , onEnter TagSearchAdd
                    ]
                    []
                ]
            , Grid.cell [ size All 1 ]
                [ Button.render Mdl
                    [ 4, 1 ]
                    model.mdl
                    [ Button.fab
                    , Options.onClick TagSearch
                    ]
                    [ Icon.i "zoom_in" ]
                ]
            ]
        , Options.styled div
            [ Options.css "margin-left" "5px"
            ]
          <|
            List.map (\tag -> searchTagChip tag) model.filters.tags
        , Button.render Mdl
            [ 4, 2 ]
            model.mdl
            [ Button.ripple
            , Button.colored
            , Button.flat
            , Button.link "https://goo.gl/forms/0wUWEPzVn212FTNg1"
            ]
            [ text "Não encontrou o que queria? Faça seu pedido!" ]
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

        maxPage =
            ceiling <| toFloat page.count / 12

        numRange =
            if page.actual < 5 then
                List.range 1 <| min maxPage 10
            else if page.actual > maxPage - 4 then
                List.range (maxPage - 9) <| maxPage
            else
                List.range (page.actual - 5) <| min maxPage (page.actual + 4)

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
                numRange
    in
        Grid.grid [] <|
            (if page.previous /= Nothing then
                [ pageButton model
                    0
                    (Options.onClick <| ChangePage <| page.actual - 1)
                    (Icon.view "chevron_left" [ Icon.size18 ])
                ]
             else
                []
            )
                ++ numberButtons
                ++ (if page.next /= Nothing then
                        [ pageButton model
                            1
                            (Options.onClick <| ChangePage <| page.actual + 1)
                            (Icon.view "chevron_right" [ Icon.size18 ])
                        ]
                    else
                        []
                   )
