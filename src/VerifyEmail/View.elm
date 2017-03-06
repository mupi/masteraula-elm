module VerifyEmail.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (id, type_, for, value, class)
import VerifyEmail.Types exposing (..)
import Material.Button as Button
import Material.Options as Options exposing (css)
import Material.Grid exposing (grid, cell, size, offset, Device(..))
import Material.Typography as Typo
import Material.Snackbar as Snackbar
import Login.Types as Login
import Login.View as Login


view : Model -> Html Msg
view model =
    div []
        [ grid []
            [ if model.success == "" then
                (cell [ size All 6, offset All 3, size Phone 12 ]
                    [ Options.styled h1
                        [ Typo.display1, Typo.center ]
                        [ text "Confirme seu email" ]
                    , div
                        [ class "text-alert danger" ]
                        [ text model.error
                        ]
                    ]
                )
              else
                (cell [ size All 6, offset All 3, size Phone 12 ]
                    [ Options.styled h1
                        [ Typo.display1, Typo.center ]
                        [ text "Confirme seu email" ]
                    , div
                        [ class "text-alert success" ]
                        [ text model.success
                        ]
                    , Html.map LoginMsg <| (Login.viewForm model.login)
                    ]
                )
            ]
        , div [ class "banner-bg" ]
            [ Options.styled h1
                [ Typo.display1, Typo.center ]
                [ text "Já possui login? Clique aqui para entrar" ]
            ]
        , Snackbar.view model.snackbar |> Html.map Snackbar
        ]
