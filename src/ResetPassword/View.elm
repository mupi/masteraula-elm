module ResetPassword.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (id, type_, for, value, class)
import ResetPassword.Types exposing (..)
import Material.Button as Button
import Material.Options as Options exposing (css, cs)
import Material.Grid exposing (grid, cell, size, offset, Device(..))
import Material.Typography as Typo
import Material.Snackbar as Snackbar
import Material.Textfield as Textfield
import Material.Typography as Typo
import Material.Color as Color
import Utils.MDLUtils as Utils
import Material.Button as Button
import Login.Types as Login
import Login.View as Login


view : Model -> Html Msg
view model =
    div []
        [ grid []
            [ cell [ size All 6, offset All 3, size Phone 12 ]
                [ Options.styled h1
                    [ Typo.display1, Typo.center ]
                    [ text "Redefinição de senha" ]
                , Options.div []
                    [ Textfield.render Mdl
                        [ 0 ]
                        model.mdl
                        [ Textfield.label "Nova senha"
                        , Textfield.password
                        , Textfield.floatingLabel
                        , Textfield.value model.newPassword
                        , Options.onInput SetNewPassword
                        ]
                        []
                    ]
                , Options.div []
                    [ Textfield.render Mdl
                        [ 1 ]
                        model.mdl
                        [ Textfield.label "Confirmar senha"
                        , Textfield.floatingLabel
                        , Textfield.password
                        , Textfield.value model.confirmPassword
                        , Options.onInput SetConfirmationPassword
                        , Utils.onEnter Reset
                        ]
                        []
                    ]
                , Button.render Mdl
                    [ 2 ]
                    model.mdl
                    [ Button.raised
                    , Button.colored
                    , Options.onClick Reset
                    ]
                    [ text "Redefinir senha" ]
                , Options.div
                    [ cs "text-alert danger" ]
                    [ text <|
                        if model.success == "" then
                            model.error
                        else
                            model.success
                    ]
                ]
            ]
        , Snackbar.view model.snackbar |> Html.map Snackbar
        ]
