module Signup.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (id, type_, for, value, class)
import Html.Events exposing (..)
import Json.Decode as Json
import Signup.Types exposing (..)
import Material.Textfield as Textfield
import Material.Button as Button
import Material.Options as Options exposing (css)
import Material.Grid exposing (grid, cell, size, offset, Device(..))
import Material.Typography as Typo
import Material.Color as Color
import Utils.MDLUtils as Utils


view : Model -> Html Msg
view model =
    div []
        [ grid []
            [ cell [ size All 6, offset All 3, size Phone 12 ]
                [ Options.styled h1
                    [ Typo.display1, Typo.center ]
                    [ text "Cadastrar no PrePaula" ]
                , div []
                    [ Textfield.render Mdl
                        [ 2 ]
                        model.mdl
                        [ Textfield.label "Username"
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
                        [ Textfield.label "Email"
                        , Textfield.text_
                        , Textfield.floatingLabel
                        , Textfield.value model.email
                        , Options.onInput SetEmail
                        ]
                        []
                    ]
                , div []
                    [ Textfield.render Mdl
                        [ 4 ]
                        model.mdl
                        [ Textfield.label "Senha"
                        , Textfield.floatingLabel
                        , Textfield.password
                        , Textfield.value model.password
                        , Options.onInput SetPassword
                        ]
                        []
                    ]
                , div []
                    [ Textfield.render Mdl
                        [ 5 ]
                        model.mdl
                        [ Textfield.label "Confirmar Senha"
                        , Textfield.floatingLabel
                        , Textfield.password
                        , Textfield.value model.confirmPassword
                        , Options.onInput SetConfirmPassword
                        , Utils.onEnter Signup
                        ]
                        []
                    ]
                , div [ class "text-alert" ]
                    [ text model.error ]
                , div [ class "text-alert" ]
                    [ text
                        (case model.success of
                            True ->
                                "Usuário cadastrado com sucesso"

                            False ->
                                ""
                        )
                    ]
                , div []
                    [ Button.render Mdl
                        [ 2 ]
                        model.mdl
                        [ Button.raised
                        , Button.colored
                        , Options.onClick Signup
                        ]
                        [ text "Sign up" ]
                    ]
                ]
            ]
        ]
