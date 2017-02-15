module VerifyEmail.State exposing (init, update)

import Http
import VerifyEmail.Types exposing (..)
import VerifyEmail.Rest exposing (..)
import Material


init : Model
init =
    Model "" "" "" Material.model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        VerifyKey recievedKey ->
            { model | key = recievedKey } ! []

        VerifyEmail ->
            ( model, fetchVerifyEmail model )

        OnFetchVerifyEmail (Ok verifyEmail) ->
            { model | success = "Email verificado com sucesso!" } ! []

        OnFetchVerifyEmail (Err error) ->
            let
                errorMsg =
                    case error of
                        Http.NetworkError ->
                            "Erro de conexão com o servidor"

                        Http.BadStatus response ->
                            toString error

                        _ ->
                            toString error
            in
                { model | error = errorMsg } ! []

        NoOp ->
            model ! []

        Mdl msg_ ->
            Material.update Mdl msg_ model
