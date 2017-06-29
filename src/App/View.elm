module App.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, src)
import App.Drawer exposing (..)
import App.Types exposing (..)
import Login.View as Login
import Login.Types as Login
import VerifyEmail.View as VerifyEmail
import ResetPassword.View as ResetPassword
import Question.View as Question
import Signup.View as Signup
import User.View as User
import App.Routing exposing (Route(..))
import Material.Options as Options exposing (css)
import Material.Layout as Layout
import Material.Button as Button
import Material.Typography as Typo
import Material.Options as Options
import Material.Grid exposing (grid, cell, size, offset, Device(..))
import Material.Card as Card
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

        UserRoute ->
            Html.map UserMsg (User.view User.viewProfile model.user)

        UserOtherRoute userId ->
            Html.map UserMsg (User.view User.viewOtherProfile model.user)

        UserUpdateRoute ->
            Html.map UserMsg (User.view User.viewUpdateProfile model.user)

        LoginRoute ->
            Html.map LoginMsg (Login.view model.login)

        SignupRoute ->
            Html.map SignupMsg (Signup.view model.signup)

        VerifyEmailRoute emailKey ->
            Html.map VerifyEmailMsg (VerifyEmail.view model.verifyEmail)

        ResetPasswordRoute codUser key ->
            Html.map ResetPasswordMsg (ResetPassword.view model.resetPassword)

        ResetPasswordEmailRoute ->
            Html.map ResetPasswordMsg (ResetPassword.view model.resetPassword)

        QuestionsRoute questionRoute ->
            Html.map QuestionMsg (Question.view model.question)

        RedirectRoute redirectRoute ->
            Html.map LoginMsg (Login.view model.login)

        NotFoundRoute ->
            notFoundView model

        RedirectRouteAux a ->
            notFoundView model


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
                    [ Layout.href "#" ]
                    [ Layout.title [] [ text "MasterAula" ] ]
                , Layout.spacer
                , Layout.navigation []
                    [ Layout.link
                        [ Layout.href "#" ]
                        [ span [] [ text "Home" ] ]
                    , Layout.link
                        [ Layout.href "#login" ]
                        [ span [] [ text "Entrar" ] ]
                    ]
                ]
            ]
        else
            [ Layout.row
                [ Options.nop ]
                [ Layout.link
                    [ Layout.href "#" ]
                    [ Layout.title [] [ text "MasterAula" ] ]
                , Layout.spacer
                , Layout.navigation []
                    [ Layout.link
                        [ Layout.href "#" ]
                        [ span [] [ text "Home" ] ]
                    , Layout.link
                        [ Layout.href "#questions/questionlist" ]
                        [ text "Preparar prova" ]
                    , Layout.link
                        [ Layout.href "#users" ]
                        [ text "Minha conta" ]
                    , Layout.link
                        [ Options.onClick (LoginMsg Login.Logout)
                        , css "cursor" "pointer"
                        ]
                        [ text "Sair ", Icon.i "exit_to_app" ]
                    ]
                ]
            ]


index : Html Msg
index =
    div [ class "container" ]
        [ div [ class "banner-header" ]
            [ Options.styled h1
                [ Typo.display2, Typo.center ]
                [ text "Biblioteca inteligente de questões" ]
            , grid []
                [ cell [ size All 8 ]
                    [ Options.styled p
                        [ Typo.display1 ]
                        [ text "MasterAula é uma ferramenta feita por educadores para educadores onde você encontra milhares de questões para montar provas e testes. " ]
                    ]
                , cell [ size All 4 ]
                    [ signupCard ]
                ]
            ]
        , grid []
            [ cell [ size All 4 ]
                [ div [ class "thumb-circle" ]
                    [ img [ src "/static/img/money.png" ] []
                    ]
                , h4 [] [ text "Venda suas questões" ]
                , p [] [ text "Ganhe uma renda extra vendendo os materiais que você já criou para outros educadores da comunidade." ]
                ]
            , cell [ size All 4 ]
                [ div [ class "thumb-circle" ]
                    [ img [ src "/static/img/clock.png" ] []
                    ]
                , h4 [] [ text "Monte provas e testes em 5 minutos!" ]
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



-- write a function that takes the model and returns
-- links according to model.page, then call that function in the drawer


signupCard : Html Msg
signupCard =
    Card.view
        []
        [ Card.title
            [ css "flex-direction" "column" ]
            [ Card.head []
                [ text "Peça um convite" ]
            , Card.subhead []
                [ text "Por enquanto a ferramenta está aberta só para convidados, caso tenha interesse em participar responda ao formulário abaixo que enviaremos um convite assim que possível" ]
            , Layout.link
                [ Layout.href "https://docs.google.com/forms/d/e/1FAIpQLSd1mB8fOBqxhGR5rWceGc9vXhRcIVDzsFdtDXJDLEt2jr_9ZA/viewform?c=0&w=1"
                , Options.cs "mdl-button mdl-js-button mdl-button--raised mdl-js-ripple-effect mdl-button--accent"
                , Options.css "line-height" "36px"
                , Options.css "margin" "20px auto"
                , Options.css "color" "#fff"
                ]
                [ text "Pedir convite" ]
            ]
        ]


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
                        [ Layout.href "https://docs.google.com/forms/d/e/1FAIpQLSd1mB8fOBqxhGR5rWceGc9vXhRcIVDzsFdtDXJDLEt2jr_9ZA/viewform?c=0&w=1"
                        , Options.onClick (Layout.toggleDrawer Mdl)
                        ]
                        [ text "Pedir convite" ]
                    ]

            LoggedIn ->
                Layout.navigation
                    []
                    [ Layout.link
                        [ Layout.href "#questions/1"
                        , Options.onClick (Layout.toggleDrawer Mdl)
                        ]
                        [ Icon.view "view_module" [ Icon.size18 ], text " Selecionar Questões" ]
                    , Layout.link
                        [ Layout.href "#questions/user_lists/1"
                        , Options.onClick (Layout.toggleDrawer Mdl)
                        ]
                        [ Icon.view "favorite" [ Icon.size18 ], text " Minhas listas" ]
                    , Layout.link
                        [ Layout.href "#users"
                        , Options.onClick (Layout.toggleDrawer Mdl)
                        ]
                        [ Icon.view "person" [ Icon.size18 ], text " Alterar conta" ]
                    ]

            QuestionDefault ->
                Html.map QuestionMsg (Question.drawerLink model.question)

            UsersView ->
                Layout.navigation
                    []
                    [ Layout.link
                        [ Layout.href "#questions/1"
                        , Options.onClick (Layout.toggleDrawer Mdl)
                        ]
                        [ Icon.view "view_module" [ Icon.size18 ], text " Selecionar Questões" ]
                    , Layout.link
                        [ Layout.href "#questions/user_lists/1"
                        , Options.onClick (Layout.toggleDrawer Mdl)
                        ]
                        [ Icon.view "favorite" [ Icon.size18 ], text " Minhas listas" ]
                    , Layout.link
                        [ Layout.href "#users"
                        , Options.onClick (Layout.toggleDrawer Mdl)
                        ]
                        [ Icon.view "person" [ Icon.size18 ], text " Alterar conta" ]
                    ]


drawer : Model -> List (Html Msg)
drawer model =
    [ Layout.title [] [ text "MasterAula" ]
    , getDrawerLinks model
    ]


notFoundView : Model -> Html Msg
notFoundView model =
    Options.div [ Options.cs "not-found" ]
        [ Options.styled p
            [ Typo.display4
            , Typo.center
            ]
            [ text "404" ]
        , Options.styled p
            [ Typo.display3
            , Typo.center
            ]
            [ text "Página não encontrada" ]
        , Layout.link
            [ Layout.href "#"
            , Options.center
            ]
            [ Button.render Mdl
                [ 0 ]
                model.mdl
                [ Button.raised
                , Button.colored
                ]
                [ text "Home" ]
            ]
        ]
