module Signup.State exposing (init, update)

import Http
import Signup.Types exposing (..)
import Signup.Rest exposing (..)
import Material
import Material.Snackbar as Snackbar
import Material.Helpers exposing (map1st, map2nd)
import Utils.StringUtils as Utils


init : Model
init =
    Model "" "" "" "" "" False Material.model Snackbar.model


verifyFields : Model -> ( Bool, String )
verifyFields model =
    if
        String.isEmpty model.username
            || String.isEmpty model.email
            || String.isEmpty model.password
            || String.isEmpty model.confirmPassword
    then
        ( False, "Por favor, preencha todos os campos" )
    else if (Utils.validateEmail model.email == False) then
        ( False, "Por favor, insira um e-mail válido" )
    else if model.password /= model.confirmPassword then
        ( False, "A senha e a confirmação devem ser iguais" )
    else if String.length model.password < 8 then
        ( False, "A senha deve conter ao menos 8 caracteres" )
    else
        ( True, "" )


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
            let
                ( verified, errorMsg ) =
                    verifyFields model

                ( snackbar, effect ) =
                    Snackbar.add (Snackbar.snackbar 0 errorMsg "Fechar") model.snackbar
                        |> map2nd (Cmd.map Snackbar)
            in
                if verified then
                    ( model, fetchSignUp model )
                else
                    { model | snackbar = snackbar } ! [ effect ]

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
                            in
                                if errors.email /= [] then
                                    "Já existe um usuário com este email cadastrado"
                                else if errors.username /= [] then
                                    "Já existe um usuário com este nome de usuário"
                                else if errors.password1 /= [] then
                                    "Senha muito simples"
                                else
                                    ""

                        _ ->
                            "Erro indefinido"

                ( snackbar, effect ) =
                    Snackbar.add (Snackbar.snackbar 0 errorMsg "Fechar") model.snackbar
                        |> map2nd (Cmd.map Snackbar)
            in
                { model | error = errorMsg, snackbar = snackbar } ! [ effect ]

        NoOp ->
            model ! []

        Snackbar (Snackbar.End a) ->
            { model | snackbar = Snackbar.model } ! []

        Snackbar msg ->
            Snackbar.update msg model.snackbar
                |> map1st (\s -> { model | snackbar = s })
                |> map2nd (Cmd.map Snackbar)

        Mdl msg_ ->
            Material.update Mdl msg_ model
