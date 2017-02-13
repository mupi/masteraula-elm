module App.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)
import App.Types exposing (..)
import Login.View as Login
import Login.Types as Login
import Question.View as Question
import User.View as User
import App.Routing exposing (Route(..))
import Material.Button as Button
import Material.Options as Options exposing (css)
import Material.Layout as Layout
import Material.Typography as Typo
import Material.Options as Options
import Material.Grid exposing (grid, cell, size, offset, Device(..))


view : Model -> Html Msg
view model =
    Layout.render Mdl
        model.mdl
        [ Layout.fixedHeader
        ]
        { header = header model
        , drawer = []
        , tabs = ( [], [] )
        , main = [ page model ]
        }


page : Model -> Html Msg
page model =
    case model.route of
        Index ->
            index

        UsersRoute ->
            case model.global.user of
                Nothing ->
                    div [] []

                Just user ->
                    Html.map UserMsg (User.view user)

        LoginRoute ->
            Html.map LoginMsg (Login.view model.login)

        QuestionRoute questionId ->
            Html.map QuestionMsg (Question.view model.question)

        QuestionPageRoute questionId ->
            Html.map QuestionMsg (Question.viewQuestionPage model.question)

        QuestionTagSearchRoute questionId ->
            Html.map QuestionMsg (Question.viewQuestionPage model.question)

        NotFoundRote ->
            notFoundView


header : Model -> List (Html Msg)
header model =
    let
        user =
            model.global.user
    in
        if user == Nothing then
            [ Layout.row
                [ Options.nop ]
                [ Layout.link
                    [ Layout.href "/" ]
                    [ Layout.title [] [ text "PrePaula" ] ]
                , Layout.spacer
                , Layout.navigation []
                    [ Button.render Mdl
                        [ 0 ]
                        model.mdl
                        [ Button.flat
                        , Button.plain
                        , Options.onClick ShowIndex
                        ]
                        [ text "Home" ]
                    , Button.render Mdl
                        [ 1 ]
                        model.mdl
                        [ Button.flat
                        , Button.plain
                        , Options.onClick ShowLogin
                        ]
                        [ text "Login" ]
                    ]
                ]
            ]
        else
            [ Layout.row
                [ Options.nop ]
                [ Layout.link
                    [ Layout.href "/" ]
                    [ Layout.title [] [ text "PrePaula" ] ]
                , Layout.spacer
                , Layout.navigation []
                    [ Button.render Mdl
                        [ 0 ]
                        model.mdl
                        [ Button.flat
                        , Button.plain
                        , Options.onClick ShowIndex
                        ]
                        [ text "Home" ]
                    , Button.render Mdl
                        [ 1 ]
                        model.mdl
                        [ Button.flat
                        , Button.plain
                        , Options.onClick ShowUser
                        ]
                        [ text "My account" ]
                    , Button.render Mdl
                        [ 2 ]
                        model.mdl
                        [ Button.flat
                        , Button.plain
                        , Options.onClick (LoginMsg Login.Logout)
                        ]
                        [ text "Logout" ]
                    ]
                ]
            ]


index : Html Msg
index =
    div [ class "container" ]
        [ div [ class "banner-header" ]
            [ Options.styled h1
                [ Typo.display2, Typo.center ]
                [ text "Encontre, compre e venda planos de aula." ]
            , Options.styled p
                [ Typo.display1 ]
                [ text "BuscaAula é uma ferramenta feita por educadores para educadores onde você pode encontrar materiais gratuitos ou a um preço acessível para utilizar em suas aulas. " ]
            , button
                [ onClick ShowLogin
                , class "mdl-button mdl-js-button mdl-button--raised mdl-button--accent"
                ]
                [ text "Comece agora" ]
            ]
        , grid []
            [ cell [ size All 4 ]
                [ div [ class "thumb-circle" ]
                    [ img [ src "/static/img/money.png" ] []
                    ]
                , h4 [] [ text " Venda seus planos de aula " ]
                , p [] [ text "Ganhe uma renda extra vendendo os materiais que você já criou para outros educadores da comunidade." ]
                ]
            , cell [ size All 4 ]
                [ div [ class "thumb-circle" ]
                    [ img [ src "/static/img/clock.png" ] []
                    ]
                , h4 [] [ text "Ganhe tempo ao preparar aulas" ]
                , p [] [ text "Otimize seu tempo de preparar aulas encontrando materiais curados por educadores de todo o Brasil." ]
                ]
            , cell [ size All 4 ]
                [ div [ class "thumb-circle" ]
                    [ img [ src "/static/img/search.png" ] []
                    ]
                , h4 [] [ text "Encontre materiais online" ]
                , p [] [ text "Busque por disciplina, assunto ou tipo de material para você adaptar e utilizar com seus alunos." ]
                ]
            ]
        ]


notFoundView : Html Msg
notFoundView =
    let
        a =
            Debug.log "location" 1
    in
        div []
            [ text "404 - Page Not found!"
            ]
