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
import Utils.MDLUtils as Utils
import Material.Button as Button


view : Model -> Html Msg
view model =
    if model.reseting then
        viewResetPasswordForm model
    else
        viewInputEmailForm model


viewResetPasswordForm : Model -> Html Msg
viewResetPasswordForm model =
    Options.div []
        [ grid []
            [ cell [ size All 6, offset All 3, size Phone 12 ]
                [ Options.styled h1
                    [ Typo.display1, Typo.center ]
                    [ text "Redefinição de senha" ]
                , Options.styled p
                    []
                    [ text "Coloque a sua nova senha e também sua confirmação para renová-la." ]
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
                        if model.success then
                            "Senha renovada com sucesso"
                        else
                            model.error
                    ]
                ]
            ]
        , Snackbar.view model.snackbar |> Html.map Snackbar
        ]


viewInputEmailForm : Model -> Html Msg
viewInputEmailForm model =
    Options.div []
        [ grid []
            [ cell [ size All 6, offset All 3, size Phone 12 ]
                [ Options.styled h1
                    [ Typo.display1, Typo.center ]
                    [ text "Redefinição de senha" ]
                , Options.div []
                    [ Options.styled p
                        []
                        [ text "Informe abaixo o endereço de email o qual a conta está registrada para receber uma mensagem para a redefinição de sua senha" ]
                    , Textfield.render Mdl
                        [ 0 ]
                        model.mdl
                        [ Textfield.label "Email"
                        , Textfield.floatingLabel
                        , Textfield.value model.email
                        , Options.onInput SetEmail
                        ]
                        []
                    ]
                , Button.render Mdl
                    [ 1 ]
                    model.mdl
                    [ Button.raised
                    , Button.colored
                    , Options.onClick Send
                    ]
                    [ text "Enviar email" ]
                , Options.div
                    [ cs "text-alert danger" ]
                    [ text <|
                        if model.success then
                            "Email de alteração enviado com sucesso"
                        else
                            model.error
                    ]
                ]
            ]
        , Snackbar.view model.snackbar |> Html.map Snackbar
        ]
