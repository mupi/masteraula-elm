module Question.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (id, type_, for, value, class)
import Html.Events exposing (..)
import Question.Types exposing (..)
import Markdown
import Material.Textfield as Textfield
import Material.Button as Button
import Material.Grid as Grid
import Material.Card as Card
import Material.Icon as Icon
import Material.Options as Options exposing (css)
import Material.Grid exposing (grid, cell, size, offset, Device(..))
import Material.Typography as Typo
import Material.Color as Color


view : Model -> Html Msg
view model =
    let
        question =
            model.question
    in
        div []
            [ questionView question
            , text (toString model)
            ]


questionView : Question -> Html Msg
questionView question =
    div []
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


questionCardView : Model -> Question -> Grid.Cell Msg
questionCardView model question =
    Grid.cell
        [ size All 4
        , Options.css "padding" "16px 16px"
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
                    [ 2, 0 ]
                    model.mdl
                    [ Button.ripple
                    , Button.accent
                    , Color.text Color.white
                    , css "font-size" "11px"
                    ]
                    [ Icon.view "favorite" [ Icon.size18 ], text " Favoritar" ]
                , Button.render Mdl
                    [ 2, 1 ]
                    model.mdl
                    [ Button.ripple
                    , Button.accent
                    , Color.text Color.white
                    , css "font-size" "11px"
                    ]
                    [ Icon.view "add" [ Icon.size18 ], text " Adicionar" ]
                ]
            ]
        ]


searchTextField : Model -> Html Msg
searchTextField model =
    div []
        [ input [ onInput TagSearchInput ] [ text model.search ]
        , button [ onClick TagSearch ] [ text "Search" ]
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
    Grid.grid []
        [ Grid.cell [ size All 3 ]
            [ searchTextField model
            ]
        , Grid.cell [ size All 9 ]
            [ Grid.grid []
                (List.map (questionCardView model) (List.take 9 model.questionPage.questions))
            , questionPageControls model
            ]
        ]
