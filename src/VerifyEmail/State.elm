module VerifyEmail.State exposing (init, update)

import Http
import VerifyEmail.Types exposing (..)
import VerifyEmail.Rest exposing (..)
import Login.State as Login
import Material


init : Model
init =
    Model "" "" "" Material.model Login.init


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
            in
                { model | error = errorMsg } ! []

        NoOp ->
            model ! []

        Mdl msg_ ->
            Material.update Mdl msg_ model

        LoginMsg subMsg ->
            let
                ( updatedLogin, cmd ) =
                    Login.update subMsg model.login
            in
                { model | login = updatedLogin } ! [ Cmd.map LoginMsg cmd ]
