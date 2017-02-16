module Login.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (id, type_, for, value, class)
import Html.Events exposing (..)
import Login.Types exposing (..)
import Material.Textfield as Textfield
import Material.Button as Button
import Material.Options as Options exposing (css)
import Material.Grid exposing (grid, cell, size, offset, Device(..))
import Material.Typography as Typo
import Material.Color as Color


-- import State exposing (..)


view : Model -> Html Msg
view model =
    div []
        [ grid []
            [ cell [ size All 6, offset All 3, size Phone 12 ] [ viewForm model ]
            ]
        , div [ class "banner-bg" ]
            [ Options.styled h1
                [ Typo.display1, Typo.center ]
                [ text "Não possui login? Cadastre-se agora" ]
            ]
        ]


viewForm : Model -> Html Msg
viewForm model =
    div []
        [ Options.styled h1
            [ Typo.display1, Typo.center ]
            [ text "Entrar no PrePaula" ]
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
                [ text "Sign in" ]
            ]
        ]
