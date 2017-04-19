module Login.View exposing (..)

import Html exposing (..)
import Login.Types exposing (..)
import Material.Textfield as Textfield
import Material.Button as Button
import Material.Options as Options exposing (css, cs)
import Material.Grid exposing (grid, cell, size, offset, Device(..))
import Material.Typography as Typo
import Material.Color as Color
import Utils.MDLUtils as Utils
import Material.Button as Button


view : Model -> Html Msg
view model =
    Options.div []
        [ grid []
            [ cell [ size All 6, offset All 3, size Phone 12 ] [ viewForm model ]
            ]
        , Options.div [ cs "banner-bg" ]
            [ Options.styled h1
                [ Typo.display1, Typo.center ]
                [ text "Não possui conta?" ]
            , Button.render Mdl
                [ 1 ]
                model.mdl
                [ Button.ripple
                , Button.colored
                , Button.link "https://docs.google.com/forms/d/e/1FAIpQLSd1mB8fOBqxhGR5rWceGc9vXhRcIVDzsFdtDXJDLEt2jr_9ZA/viewform?c=0&w=1"
                , Button.raised
                ]
                [ text "Clique aqui para pedir um convite" ]
            ]
        ]


viewForm : Model -> Html Msg
viewForm model =
    Options.div []
        [ Options.styled h1
            [ Typo.display1, Typo.center ]
            [ text "Entrar no MasterAula" ]
        , Options.div []
            [ Textfield.render Mdl
                [ 2 ]
                model.mdl
                [ Textfield.label "Usuário"
                , Textfield.text_
                , Textfield.floatingLabel
                , Textfield.value model.username
                , Options.onInput SetUsername
                ]
                []
            ]
        , Options.div []
            [ Textfield.render Mdl
                [ 3 ]
                model.mdl
                [ Textfield.label "Senha"
                , Textfield.floatingLabel
                , Textfield.password
                , Textfield.value model.password
                , Options.onInput SetPassword
                , Utils.onEnter Login
                ]
                []
            ]
        , Options.div [ cs "text-alert" ]
            [ text model.error ]
        , Options.div []
            [ Button.render Mdl
                [ 2 ]
                model.mdl
                [ Button.raised
                , Button.colored
                , Options.onClick Login
                ]
                [ text "Fazer login" ]
            , Button.render Mdl
                [ 3 ]
                model.mdl
                [ Button.raised
                , Button.colored
                , Options.onClick ResetPassword
                , css "margin-left" "10px"
                ]
                [ text "Esqueci a senha" ]
            ]
        ]
