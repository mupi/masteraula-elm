module App.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)
import App.Drawer exposing (..)
import App.Types exposing (..)
import Login.View as Login
import Login.Types as Login
import VerifyEmail.View as VerifyEmail
import Question.View as Question
import Signup.View as Signup
import User.View as User
import App.Routing exposing (Route(..))
import Material.Button as Button
import Material.Options as Options exposing (css)
import Material.Layout as Layout
import Material.Typography as Typo
import Material.Options as Options
import Material.Grid exposing (grid, cell, size, offset, Device(..))
import Material.Button as Button
import Material.Icon as Icon


view : Model -> Html Msg
view model =
    Layout.render Mdl
        model.mdl
        (case model.currentDrawerLinks of
            QuestionDefault ->
                [ Layout.fixedHeader
                , Layout.fixedDrawer
                ]

            _ ->
                [ Layout.fixedHeader
                ]
        )
        { header = header model
        , drawer = drawer model
        , tabs = ( [], [] )
        , main = [ page model ]
        }


page : Model -> Html Msg
page model =
    case model.route of
        IndexRoute ->
            index

        UsersRoute ->
            case model.global.user of
                Nothing ->
                    div [] []

                Just user ->
                    Html.map UserMsg (User.view user)

        LoginRoute ->
            Html.map LoginMsg (Login.view model.login)

        SignupRoute ->
            Html.map SignupMsg (Signup.view model.signup)

        VerifyEmailRoute emailKey ->
            Html.map VerifyEmailMsg (VerifyEmail.view model.verifyEmail)

        QuestionRoute questionId ->
            Html.map QuestionMsg (Question.view Question.viewQuestion model.question)

        QuestionPageRoute page ->
            Html.map QuestionMsg (Question.view Question.viewQuestionPage model.question)

        QuestionTagSearchRoute page ->
            Html.map QuestionMsg (Question.view Question.viewQuestionPage model.question)

        QuestionListRoute ->
            Html.map QuestionMsg (Question.view Question.viewQuestionList model.question)

        SelectedQuestionListRoute questionListId ->
            Html.map QuestionMsg (Question.view Question.viewSelectedQuestionList model.question)

        MineQuestionListRoute page ->
            Html.map QuestionMsg (Question.view Question.viewQuestionListPage model.question)

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
                [ onClick ShowSignup
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



-- write a function that takes the model and returns
-- links according to model.page, then call that function in the drawer


getDrawerLinks : Model -> Html Msg
getDrawerLinks model =
    let
        currentDrawerLinks =
            model.currentDrawerLinks
    in
        case currentDrawerLinks of
            HomeDefault ->
                Layout.navigation
                    []
                    [ Layout.link
                        [ Layout.href "#login"
                        , Options.onClick (Layout.toggleDrawer Mdl)
                        ]
                        [ text "Entrar" ]
                    , Layout.link
                        [ Layout.href "#signup"
                        , Options.onClick (Layout.toggleDrawer Mdl)
                        ]
                        [ text "Fazer cadastro" ]
                      -- , Layout.link
                      --     [ Layout.href "#questions/1"
                      --     , Options.onClick (Layout.toggleDrawer Mdl)
                      --     ]
                      --     [ text "Ver questões" ]
                    ]

            LoggedIn ->
                Layout.navigation
                    []
                    [ Layout.link
                        [ Layout.href "#questions/1"
                        , Options.onClick (Layout.toggleDrawer Mdl)
                        ]
                        [ text "Ver questões" ]
                      -- , Layout.link
                      --     [ Layout.href "#"
                      --     , Options.onClick (Layout.toggleDrawer Mdl)
                      --     ]
                      --     [ text "Minhas questões" ]
                    , Layout.link
                        [ Layout.href "#questions/minequestionlists/1"
                        , Options.onClick (Layout.toggleDrawer Mdl)
                        ]
                        [ text "Minhas listas" ]
                    , Layout.link
                        [ Layout.href "#users"
                        , Options.onClick (Layout.toggleDrawer Mdl)
                        ]
                        [ text "Alterar conta" ]
                    ]

            QuestionDefault ->
                Html.map QuestionMsg (Question.drawerLink model.question)

            UsersView ->
                Layout.navigation
                    []
                    [ Layout.link
                        [ Layout.href "#"
                        , Options.onClick (Layout.toggleDrawer Mdl)
                        ]
                        [ text "Minhas questões" ]
                    , Layout.link
                        [ Layout.href "#questions/minequestionlists/1"
                        , Options.onClick (Layout.toggleDrawer Mdl)
                        ]
                        [ text "Minhas listas" ]
                    , Layout.link
                        [ Layout.href "#"
                        , Options.onClick (Layout.toggleDrawer Mdl)
                        ]
                        [ text "Alterar conta" ]
                    ]


drawer : Model -> List (Html Msg)
drawer model =
    [ Layout.title [] [ text "PrePaula" ]
    , getDrawerLinks model
    ]
