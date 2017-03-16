module User.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (id, type_, for, value, class)
import Html.Events exposing (..)
import User.Types exposing (..)
import Material.Textfield as Textfield
import Material.Button as Button
import Material.Icon as Icon
import Material.Options as Options exposing (css, cs)
import Material.Grid exposing (grid, cell, size, offset, Device(..))
import Material.Typography as Typo
import Material.Snackbar as Snackbar
import Material.Color as Color
import Utils.MDLUtils as Utils


-- import State exposing (..)


view : (Model -> Html Msg) -> Model -> Html Msg
view method model =
    Options.div []
        [ method model
        , Snackbar.view model.snackbar |> Html.map Snackbar
        ]


profile : Model -> User -> Html Msg
profile model user =
    grid []
        [ cell [ size All 3, offset All 1 ]
            [ Options.div []
                [ Options.div
                    [ css "text-align" "center" ]
                    [ Icon.view "person" [ Icon.size36 ] ]
                , Options.styled p
                    [ Typo.headline ]
                    [ text user.name ]
                , Options.styled p
                    [ Typo.title ]
                    [ text user.username ]
                , hr [] []
                , Options.div [ Typo.subhead ]
                    [ Icon.view "email" [ Icon.size18 ]
                    , text (String.concat [ " ", user.email ])
                    ]
                , if model.user.id == user.id then
                    Button.render Mdl
                        [ 0 ]
                        model.mdl
                        [ Options.onClick ProfileEdit
                        ]
                        [ text "Editar conta" ]
                  else
                    Options.span [] []
                ]
            ]
        , cell [ size All 8 ] [ viewInfo model ]
        ]


viewProfile : Model -> Html Msg
viewProfile model =
    let
        user =
            model.user
    in
        profile model user


viewOtherProfile : Model -> Html Msg
viewOtherProfile model =
    let
        user =
            model.otherUser
    in
        profile model user


viewInfo : Model -> Html Msg
viewInfo model =
    Options.div [] []



-- Update Profile


viewUpdateProfile : Model -> Html Msg
viewUpdateProfile model =
    let
        user =
            model.editUser
    in
        Options.div []
            [ grid []
                [ cell [ size All 6, offset All 3, size Phone 12 ]
                    [ Options.styled h1
                        [ Typo.display1, Typo.center ]
                        [ text "Editar a Conta" ]
                    , Options.div []
                        [ Textfield.render Mdl
                            [ 0, 0 ]
                            model.mdl
                            [ Textfield.label "Nome"
                            , Textfield.text_
                            , Textfield.floatingLabel
                            , Textfield.value user.name
                            , Options.onInput SetName
                            ]
                            []
                        ]
                    , Options.div []
                        [ Button.render Mdl
                            [ 0, 2 ]
                            model.mdl
                            [ Button.raised
                            , Button.colored
                            , Options.onClick ProfileUpdate
                            ]
                            [ text "Editar Conta" ]
                        , Button.render Mdl
                            [ 0, 3 ]
                            model.mdl
                            [ Button.raised
                            , Button.colored
                            , Options.onClick ProfileSee
                            ]
                            [ text "Cancelar" ]
                        ]
                    , hr [] []
                    , Options.styled h1
                        [ Typo.display1, Typo.center ]
                        [ text "Trocar a senha" ]
                    , Options.div []
                        [ Textfield.render Mdl
                            [ 1, 0 ]
                            model.mdl
                            [ Textfield.label "Senha atual"
                            , Textfield.text_
                            , Textfield.floatingLabel
                            , Textfield.password
                            , Textfield.value model.password
                            , Options.onInput SetPassword
                            ]
                            []
                        ]
                    , Options.div []
                        [ Textfield.render Mdl
                            [ 1, 1 ]
                            model.mdl
                            [ Textfield.label "Nova senha"
                            , Textfield.text_
                            , Textfield.floatingLabel
                            , Textfield.password
                            , Textfield.value model.newPassword
                            , Options.onInput SetNewPassword
                            ]
                            []
                        ]
                    , Options.div []
                        [ Textfield.render Mdl
                            [ 1, 2 ]
                            model.mdl
                            [ Textfield.label "Confirmar senha"
                            , Textfield.text_
                            , Textfield.floatingLabel
                            , Textfield.password
                            , Textfield.value model.confirmPassword
                            , Options.onInput SetConfirmPassword
                            ]
                            []
                        ]
                    , Options.div []
                        [ Button.render Mdl
                            [ 1, 3 ]
                            model.mdl
                            [ Button.raised
                            , Button.colored
                            , Options.onClick PasswordChange
                            ]
                            [ text "Alterar senha" ]
                        , Button.render Mdl
                            [ 1, 4 ]
                            model.mdl
                            [ Button.raised
                            , Button.colored
                            , Options.onClick ProfileSee
                            ]
                            [ text "Cancelar" ]
                        ]
                    ]
                ]
            ]
