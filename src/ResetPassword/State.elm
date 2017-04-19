module ResetPassword.State exposing (init, update)

import Http
import ResetPassword.Types exposing (..)
import ResetPassword.Rest exposing (..)
import Login.State as Login
import Material
import Material.Helpers exposing (map1st, map2nd)
import Material.Snackbar as Snackbar


init : Model
init =
    Model "" "" "" "" "" False "" False Snackbar.model Material.model


verifyPassword : Model -> ( Bool, String )
verifyPassword model =
    if
        String.isEmpty model.confirmPassword
            || String.isEmpty model.newPassword
    then
        ( False, "Por favor, preencha todos os campos" )
    else if String.length model.newPassword < 8 then
        ( False, "A senha deve conter ao menos 8 caracteres" )
    else if model.newPassword /= model.confirmPassword then
        ( False, "A senha e a confirmação devem ser iguais" )
    else
        ( True, "" )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SendEmail ->
            { model | email = "", reseting = False, error = "" } ! []

        SetEmail newEmail ->
            { model | email = newEmail } ! []

        ResetPassword codUser key ->
            { model | codUser = codUser, key = key, success = False, reseting = True, error = "" } ! []

        SetNewPassword newPassword ->
            { model | newPassword = newPassword } ! []

        SetConfirmationPassword newConfirmationPassword ->
            { model | confirmPassword = newConfirmationPassword } ! []

        Send ->
            { model | error = "", success = False } ! [ fetchSendEmail model ]

        Reset ->
            let
                ( verified, errorMsg ) =
                    verifyPassword model
            in
                if verified then
                    { model | error = "", success = False } ! [ fetchResetPassword model ]
                else
                    { model | error = errorMsg, success = False } ! []

        OnFetchSendEmail (Ok resetPassword) ->
            { model | success = True, error = "" } ! []

        OnFetchSendEmail (Err error) ->
            let
                errorMsg =
                    case error of
                        Http.NetworkError ->
                            "Erro de conexão com o servidor"

                        Http.BadStatus response ->
                            "Informe um email válido"

                        _ ->
                            toString error

                ( snackbar, effect ) =
                    Snackbar.add (Snackbar.snackbar 0 errorMsg "Fechar") model.snackbar
                        |> map2nd (Cmd.map Snackbar)
            in
                { model | error = errorMsg, snackbar = snackbar } ! [ effect ]

        OnFetchResetPassword (Ok resetPassword) ->
            { model | success = True, error = "" } ! []

        OnFetchResetPassword (Err error) ->
            let
                errorMsg =
                    case error of
                        Http.NetworkError ->
                            "Erro de conexão com o servidor"

                        Http.BadStatus response ->
                            "Código de verificação inválido"

                        _ ->
                            toString error

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
