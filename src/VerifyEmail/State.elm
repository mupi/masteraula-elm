module VerifyEmail.State exposing (init, update)

import Http
import VerifyEmail.Types exposing (..)
import VerifyEmail.Rest exposing (..)
import Login.State as Login
import Material
import Material.Helpers exposing (map1st, map2nd)
import Material.Snackbar as Snackbar


init : Model
init =
    Model "" "" "" Login.init Snackbar.model Material.model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        VerifyKey recievedKey ->
            let
                newModel =
                    { model | key = recievedKey, success = "" }
            in
                newModel ! [ fetchVerifyEmail newModel ]

        OnFetchVerifyEmail (Ok verifyEmail) ->
            { model | success = "Email verificado com sucesso!" } ! []

        OnFetchVerifyEmail (Err error) ->
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

        LoginMsg subMsg ->
            let
                ( updatedLogin, cmd ) =
                    Login.update subMsg model.login
            in
                { model | login = updatedLogin } ! [ Cmd.map LoginMsg cmd ]

        Snackbar (Snackbar.End a) ->
            { model | snackbar = Snackbar.model } ! []

        Snackbar msg ->
            Snackbar.update msg model.snackbar
                |> map1st (\s -> { model | snackbar = s })
                |> map2nd (Cmd.map Snackbar)

        Mdl msg_ ->
            Material.update Mdl msg_ model
