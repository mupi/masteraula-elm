module Signup.State exposing (init, update)

import Http
import Signup.Types exposing (..)
import Signup.Rest exposing (..)
import Material


init : Model
init =
    Model "" "" "" "" "" False Material.model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetUsername newUsername ->
            { model | username = newUsername } ! []

        SetPassword newPassword ->
            { model | password = newPassword } ! []

        SetConfirmPassword newConfirmPassword ->
            { model | confirmPassword = newConfirmPassword } ! []

        SetEmail newEmail ->
            { model | email = newEmail } ! []

        Signup ->
            ( model, fetchSignUp model )

        OnFetchSignup (Ok errors) ->
            { model | success = True, error = "" } ! []

        OnFetchSignup (Err error) ->
            let
                errorMsg =
                    case error of
                        Http.NetworkError ->
                            "Erro de conexão com o servidor"

                        Http.BadStatus response ->
                            let
                                errors =
                                    decodeErrors response.body

                                error =
                                    if errors.email /= [] then
                                        List.head errors.email
                                    else if errors.username /= [] then
                                        List.head errors.username
                                    else if errors.password1 /= [] then
                                        List.head errors.password1
                                    else if errors.password2 /= [] then
                                        List.head errors.password2
                                    else if errors.non_field_errors /= [] then
                                        List.head errors.non_field_errors
                                    else
                                        Nothing
                            in
                                case error of
                                    Just error ->
                                        error

                                    Nothing ->
                                        ""

                        _ ->
                            "Erro indefinido"
            in
                { model | error = errorMsg } ! []

        NoOp ->
            model ! []

        Mdl msg_ ->
            Material.update Mdl msg_ model
