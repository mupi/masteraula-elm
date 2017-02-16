port module App.State exposing (init, update, subscriptions)

import Navigation exposing (Location)
import App.Routing exposing (parseLocation, Route(..))
import App.Types exposing (..)
import Login.State as Login
import Signup.State as Signup
import VerifyEmail.State as VerifyEmail
import VerifyEmail.Types as VerifyEmail
import Question.State as Question
import Question.Types as Question
import User.State as User
import Material


port setStorage : GlobalStorage -> Cmd msg


init : Maybe GlobalStorage -> Location -> ( Model, Cmd Msg )
init savedGlobal location =
    let
        currentRoute =
            parseLocation Nothing location

        mdl =
            Material.model

        initModel =
            (Model
                Login.init
                Signup.init
                VerifyEmail.init
                Question.init
                currentRoute
                (globalInit savedGlobal)
                mdl
            )
    in
        update (OnLocationChange location) initModel


globalInit : Maybe GlobalStorage -> Global
globalInit savedGlobal =
    Maybe.withDefault (Global Nothing Nothing) savedGlobal


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UserMsg subMsg ->
            let
                global =
                    model.global

                ( user, cmd ) =
                    case global.user of
                        Nothing ->
                            ( Nothing, Cmd.none )

                        Just user ->
                            let
                                ( updatedUser, cmd ) =
                                    User.update subMsg user
                            in
                                ( Just updatedUser, cmd )

                newGlobal =
                    { global | user = user }
            in
                ( { model | global = newGlobal }, Cmd.map UserMsg cmd )

        LoginMsg subMsg ->
            let
                ( updatedLogin, cmd ) =
                    Login.update subMsg model.login

                newGlobal =
                    if updatedLogin.user == Nothing then
                        Global Nothing Nothing
                    else
                        Global updatedLogin.user updatedLogin.token

                newCmd =
                    if updatedLogin.user == Nothing then
                        Cmd.map LoginMsg cmd
                    else
                        Navigation.newUrl "#index"
            in
                ( { model | login = updatedLogin, global = newGlobal }, Cmd.batch [ setStorage newGlobal, newCmd ] )

        SignupMsg subMsg ->
            let
                ( updatedSignup, cmd ) =
                    Signup.update subMsg model.signup
            in
                ( { model | signup = updatedSignup }, Cmd.map SignupMsg cmd )

        VerifyEmailMsg subMsg ->
            let
                ( updatedVerifyKey, cmd ) =
                    VerifyEmail.update subMsg model.verifyEmail

                -- Trata o login que dá para ser feito através da página verifyEmail
                updatedLogin =
                    updatedVerifyKey.login

                newGlobal =
                    if updatedLogin.user == Nothing then
                        Global Nothing Nothing
                    else
                        Global updatedLogin.user updatedLogin.token

                newCmd =
                    if updatedLogin.user == Nothing then
                        Cmd.map VerifyEmailMsg cmd
                    else
                        Navigation.newUrl "#index"

                -- Serve para fazer um "logout" no login que está no verifyEmail para evitar um loop acima
                newVerifyKey =
                    if updatedLogin.user == Nothing then
                        updatedVerifyKey
                    else
                        { updatedVerifyKey | login = Login.init }
            in
                ( { model | verifyEmail = newVerifyKey, global = newGlobal }, Cmd.batch [ setStorage newGlobal, newCmd ] )

        QuestionMsg subMsg ->
            let
                ( updatedQuestion, cmd ) =
                    Question.update subMsg model.question model.global
            in
                ( { model | question = updatedQuestion }, Cmd.map QuestionMsg cmd )

        OnLocationChange location ->
            let
                newRoute =
                    parseLocation model.global.user location

                ( newModel, cmd ) =
                    case newRoute of
                        QuestionRoute questionId ->
                            let
                                ( updatedQuestion, cmd ) =
                                    Question.update (Question.GetQuestion questionId) model.question model.global
                            in
                                ( { model | question = updatedQuestion }, Cmd.map QuestionMsg cmd )

                        QuestionPageRoute page ->
                            let
                                ( updatedQuestion, cmd ) =
                                    Question.update (Question.GetQuestionPage page) model.question model.global
                            in
                                ( { model | question = updatedQuestion }, Cmd.map QuestionMsg cmd )

                        QuestionTagSearchRoute page ->
                            let
                                ( updatedQuestion, cmd ) =
                                    Question.update (Question.GetQuestionTagSearch page) model.question model.global
                            in
                                ( { model | question = updatedQuestion }, Cmd.map QuestionMsg cmd )

                        VerifyEmailRoute emailKey ->
                            let
                                ( updatedKey, cmd ) =
                                    VerifyEmail.update (VerifyEmail.VerifyKey emailKey) model.verifyEmail
                            in
                                ( { model | verifyEmail = updatedKey }, Cmd.map VerifyEmailMsg cmd )

                        _ ->
                            ( model, Cmd.none )
            in
                ( { newModel | route = newRoute }, cmd )

        ShowIndex ->
            ( model, Navigation.newUrl "#index" )

        ShowLogin ->
            ( model, Navigation.newUrl "#login" )

        ShowSignup ->
            ( model, Navigation.newUrl "#signup" )

        ShowUser ->
            ( model, Navigation.newUrl "#users" )

        Mdl msg_ ->
            Material.update Mdl msg_ model


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
