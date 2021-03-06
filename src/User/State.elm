module User.State exposing (init, initUser, update)

import App.Types as App
import Http
import User.Types exposing (..)
import User.Rest exposing (..)
import Material
import Material.Helpers exposing (map1st, map2nd)
import Material.Snackbar as Snackbar
import Navigation


init : Model
init =
    Model initUser initUser initUser "" "" "" Snackbar.model Material.model


initUser : User
initUser =
    User 0 "" "" ""


verifyFieldsProfile : Model -> ( Bool, String )
verifyFieldsProfile model =
    let
        user =
            model.editUser
    in
        if
            String.isEmpty user.name
            -- || String.isEmpty user.email
        then
            ( False, "Por favor, preencha todos os campos" )
            -- else if (Utils.validateEmail user.email == False) then
            --     ( False, "Por favor, insira um e-mail válido" )
        else
            ( True, "" )


verifyFieldsPassword : Model -> ( Bool, String )
verifyFieldsPassword model =
    if
        String.isEmpty model.password
            || String.isEmpty model.confirmPassword
            || String.isEmpty model.newPassword
    then
        ( False, "Por favor, preencha todos os campos" )
    else if String.length model.newPassword < 8 then
        ( False, "A senha deve conter ao menos 8 caracteres" )
    else if model.newPassword /= model.confirmPassword then
        ( False, "A senha e a confirmação devem ser iguais" )
    else
        ( True, "" )


update : Msg -> Model -> App.Global -> ( Model, Cmd Msg )
update msg model global =
    case msg of
        GetUser userId ->
            model ! [ fetchGetUser userId global.token ]

        ProfileSee ->
            { model | editUser = model.user } ! [ Navigation.newUrl <| String.concat [ "#users/" ] ]

        ProfileEdit ->
            { model | editUser = model.user } ! [ Navigation.newUrl <| String.concat [ "#users/updateprofile/" ] ]

        SetName newName ->
            let
                newEditUser =
                    let
                        user =
                            model.editUser
                    in
                        { user | name = newName }
            in
                { model | editUser = newEditUser } ! []

        SetEmail newEmail ->
            let
                newEditUser =
                    let
                        user =
                            model.editUser
                    in
                        { user | email = newEmail }
            in
                { model | editUser = newEditUser } ! []

        SetPassword newPassword ->
            { model | password = newPassword } ! []

        SetNewPassword newNewPassword ->
            { model | newPassword = newNewPassword } ! []

        SetConfirmPassword newConfirmPassword ->
            { model | confirmPassword = newConfirmPassword } ! []

        ProfileUpdate ->
            let
                ( verified, errorMsg ) =
                    verifyFieldsProfile model

                ( snackbar, effect ) =
                    Snackbar.add (Snackbar.snackbar 0 errorMsg "Fechar") model.snackbar
                        |> map2nd (Cmd.map Snackbar)
            in
                if verified then
                    model ! [ fetchProfileUpdate model global.token ]
                else
                    { model | snackbar = snackbar } ! [ effect ]

        PasswordChange ->
            let
                ( verified, errorMsg ) =
                    verifyFieldsPassword model

                ( snackbar, effect ) =
                    Snackbar.add (Snackbar.snackbar 0 errorMsg "Fechar") model.snackbar
                        |> map2nd (Cmd.map Snackbar)
            in
                if verified then
                    model ! [ fetchChangePassword model global.token ]
                else
                    { model | snackbar = snackbar } ! [ effect ]

        OnFetchPasswordChange (Ok text) ->
            let
                ( snackbar, effect ) =
                    Snackbar.add (Snackbar.snackbar 0 "Senha alterada com sucesso" "Fechar") model.snackbar
                        |> map2nd (Cmd.map Snackbar)
            in
                { model | snackbar = snackbar } ! [ effect ]

        OnFetchPasswordChange (Err error) ->
            let
                errorMsg =
                    case error of
                        Http.NetworkError ->
                            "Erro de conexão com o servidor"

                        Http.BadStatus response ->
                            "Senha muito simples"

                        _ ->
                            "Erro indefinido"

                ( snackbar, effect ) =
                    Snackbar.add (Snackbar.snackbar 0 errorMsg "Fechar") model.snackbar
                        |> map2nd (Cmd.map Snackbar)
            in
                { model | snackbar = snackbar } ! [ effect ]

        OnFetchProfileUpdate (Ok text) ->
            let
                ( snackbar, effect ) =
                    Snackbar.add (Snackbar.snackbar 0 "Conta alterada com sucesso" "Fechar") model.snackbar
                        |> map2nd (Cmd.map Snackbar)
            in
                { model | user = model.editUser, snackbar = snackbar } ! [ effect ]

        OnFetchProfileUpdate (Err error) ->
            let
                errorMsg =
                    case error of
                        Http.NetworkError ->
                            "Erro de conexão com o servidor"

                        Http.BadStatus response ->
                            "Já existe um usuário cadastrado no email informado"

                        _ ->
                            "Erro indefinido"

                ( snackbar, effect ) =
                    Snackbar.add (Snackbar.snackbar 0 errorMsg "Fechar") model.snackbar
                        |> map2nd (Cmd.map Snackbar)
            in
                { model | snackbar = snackbar } ! [ effect ]

        OnFetchGetUser (Ok newUser) ->
            { model | otherUser = newUser } ! []

        OnFetchGetUser (Err error) ->
            model ! []

        Snackbar (Snackbar.End a) ->
            { model | snackbar = Snackbar.model } ! []

        Snackbar msg ->
            Snackbar.update msg model.snackbar
                |> map1st (\s -> { model | snackbar = s })
                |> map2nd (Cmd.map Snackbar)

        NoOp ->
            model ! []

        Mdl msg_ ->
            Material.update Mdl msg_ model
