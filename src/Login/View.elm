module Login.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (id, type_, for, value, class)
import Html.Events exposing (..)
import Json.Decode as Json
import Login.Types exposing (..)
import Material.Textfield as Textfield
import Material.Button as Button
import Material.Options as Options exposing (css)
import Material.Grid exposing (grid, cell, size, offset, Device(..))
import Material.Typography as Typo
import Material.Color as Color
import Utils.MDLUtils as Utils
import Material.Button as Button


view : Model -> Html Msg
view model =
    div []
        [ grid []
            [ cell [ size All 6, offset All 3, size Phone 12 ] [ viewForm model ]
            ]
        , div [ class "banner-bg" ]
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
    div []
        [ Options.styled h1
            [ Typo.display1, Typo.center ]
            [ text "Entrar no MasterAula" ]
        , div []
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
        , div []
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
        , div [ class "text-alert" ]
            [ text model.error ]
        , div []
            [ Button.render Mdl
                [ 2 ]
                model.mdl
                [ Button.raised
                , Button.colored
                , Options.onClick Login
                ]
                [ text "Fazer login" ]
            ]
        ]
