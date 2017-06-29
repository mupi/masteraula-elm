port module App.State exposing (init, update, subscriptions)

import Json.Encode as Encode
import Json.Decode as Decode exposing (..)
import Material
import Navigation exposing (Location)


-- My Modules

import App.Routing as Routing
import App.Drawer exposing (..)
import App.Rest exposing (..)
import App.Routing exposing (parseLocation, Route(..))
import App.Types exposing (..)
import App.Ports exposing (..)
import Login.State as Login
import Login.Types as Login
import Signup.State as Signup
import ResetPassword.State as ResetPassword
import ResetPassword.Types as ResetPassword
import VerifyEmail.State as VerifyEmail
import VerifyEmail.Types as VerifyEmail
import Question.State as Question
import Question.Types as Question
import Question.QuestionList.State as QuestionList
import Question.QuestionList.Types as QuestionList
import Question.Question.State as Question1
import Question.Question.Types as Question1
import User.State as User
import User.Types as User


init : Maybe String -> Location -> ( Model, Cmd Msg )
init storage location =
    let
        savedStorage =
            case storage of
                Just str ->
                    case Decode.decodeString localStorageDecoder str of
                        Ok value ->
                            Just value

                        Err error ->
                            let
                                a =
                                    Debug.log "a" error
                            in
                                Nothing

                Nothing ->
                    Nothing

        currentRoute =
            parseLocation Nothing location

        mdl =
            Material.model

        initModel =
            (Model
                Login.init
                Signup.init
                VerifyEmail.init
                ResetPassword.init
                (initQuestion savedStorage)
                (initUser savedStorage)
                currentRoute
                Nothing
                (initGlobal savedStorage)
                (initStorage savedStorage)
                HomeDefault
                mdl
            )
    in
        update (OnLocationChange location) initModel


initClear : Model
initClear =
    let
        mdl =
            Material.model
    in
        Model
            Login.init
            Signup.init
            VerifyEmail.init
            ResetPassword.init
            Question.init
            User.init
            Routing.IndexRoute
            Nothing
            (initGlobal Nothing)
            (initStorage Nothing)
            HomeDefault
            mdl


initGlobal : Maybe LocalStorage -> Global
initGlobal savedStorage =
    case savedStorage of
        Just storage ->
            Global storage.user storage.token

        Nothing ->
            Global Nothing Nothing


initQuestion : Maybe LocalStorage -> Question.Model
initQuestion savedStorage =
    let
        question =
            Question.init
    in
        case savedStorage of
            Just storage ->
                { question | questionListEdit = storage.questionList }

            Nothing ->
                question


initUser : Maybe LocalStorage -> User.Model
initUser savedStorage =
    let
        user =
            User.init
    in
        case savedStorage of
            Just storage ->
                case storage.user of
                    Just u ->
                        { user | user = u, editUser = u }

                    Nothing ->
                        user

            Nothing ->
                user


initStorage : Maybe LocalStorage -> LocalStorage
initStorage savedStorage =
    Maybe.withDefault (LocalStorage Nothing Nothing QuestionList.initQuestionList) savedStorage


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UserMsg subMsg ->
            let
                ( updatedUser, cmd ) =
                    User.update subMsg model.user model.global

                newCmd =
                    Cmd.map UserMsg cmd

                global =
                    model.global

                newGlobal =
                    { global | user = Just updatedUser.user }

                newStorage =
                    let
                        localStorage =
                            model.localStorage
                    in
                        { localStorage | user = newGlobal.user }
            in
                ( { model | user = updatedUser, global = newGlobal, localStorage = newStorage }, Cmd.batch [ setLocalStorage newStorage, newCmd ] )

        LoginMsg subMsg ->
            let
                newLoginModel =
                    let
                        loginModel =
                            model.login
                    in
                        { loginModel | redirectHash = model.redirectHash }

                ( updatedLogin, cmd ) =
                    Login.update subMsg newLoginModel

                newCmd =
                    Cmd.map LoginMsg cmd
            in
                if subMsg == Login.Logout then
                    initClear ! [ setLocalStorage (initStorage Nothing), newCmd ]
                else
                    let
                        newGlobal =
                            Global updatedLogin.user updatedLogin.token

                        newStorage =
                            let
                                localStorage =
                                    model.localStorage
                            in
                                { localStorage | user = newGlobal.user, token = newGlobal.token }

                        newUser =
                            let
                                user =
                                    model.user
                            in
                                { user | user = Maybe.withDefault User.initUser newGlobal.user }
                    in
                        { model | login = updatedLogin, user = newUser, global = newGlobal, localStorage = newStorage } ! [ setLocalStorage newStorage, newCmd ]

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

                -- Threats the login that can be done using verifyEmail page
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

                -- Do a "logout" in the verifyEmail login model to prevent a loop
                newVerifyKey =
                    if updatedLogin.user == Nothing then
                        updatedVerifyKey
                    else
                        { updatedVerifyKey | login = Login.init }

                newStorage =
                    let
                        localStorage =
                            model.localStorage
                    in
                        { localStorage | user = newGlobal.user, token = newGlobal.token }
            in
                ( { model | verifyEmail = newVerifyKey, global = newGlobal, localStorage = newStorage }, Cmd.batch [ setLocalStorage newStorage, newCmd ] )

        ResetPasswordMsg subMsg ->
            let
                ( updatedResetPassword, cmd ) =
                    ResetPassword.update subMsg model.resetPassword

                -- Threats the login that can be done using verifyEmail page
                -- updatedLogin =
                --     updatedVerifyKey.login
                --
                -- newGlobal =
                --     if updatedLogin.user == Nothing then
                --         Global Nothing Nothing
                --     else
                --         Global updatedLogin.user updatedLogin.token
                --
                newCmd =
                    Cmd.map ResetPasswordMsg cmd

                --     if updatedLogin.user == Nothing then
                --         Cmd.map VerifyEmailMsg cmd
                --     else
                --         Navigation.newUrl "#index"
                -- Do a "logout" in the verifyEmail login model to prevent a loop
                -- newVerifyKey =
                --     if updatedLogin.user == Nothing then
                --         updatedVerifyKey
                --     else
                --         { updatedVerifyKey | login = Login.init }
                --
                -- newStorage =
                --     let
                --         localStorage =
                --             model.localStorage
                --     in
                --         { localStorage | user = newGlobal.user, token = newGlobal.token }
            in
                { model | resetPassword = updatedResetPassword } ! [ newCmd ]

        QuestionMsg subMsg ->
            let
                ( updatedQuestion, cmd ) =
                    Question.update subMsg model.question model.global

                newStorage =
                    let
                        localStorage =
                            model.localStorage
                    in
                        { localStorage | questionList = updatedQuestion.questionListEdit }

                cmds =
                    Cmd.batch <|
                        [ setLocalStorage newStorage, Cmd.map QuestionMsg cmd ]
            in
                ( { model | question = updatedQuestion, localStorage = newStorage }, cmds )

        OnLocationChange location ->
            let
                newRoute =
                    parseLocation model.global.user location

                newDrawerLink =
                    parseDrawerLink model.global.user newRoute

                ( newModel, cmd ) =
                    case newRoute of
                        UserOtherRoute userId ->
                            let
                                ( updatedUser, cmd ) =
                                    User.update (User.GetUser userId) model.user model.global
                            in
                                ( { model | user = updatedUser, currentDrawerLinks = QuestionDefault }, Cmd.map UserMsg cmd )

                        QuestionRoute questionId ->
                            let
                                ( updatedQuestion, cmd ) =
                                    Question.update (Question.QuestionMsg <| Question1.GetQuestion questionId) model.question model.global
                            in
                                ( { model | question = updatedQuestion, currentDrawerLinks = QuestionDefault }, Cmd.map QuestionMsg cmd )

                        QuestionPageRoute page ->
                            let
                                ( updatedQuestion, cmd ) =
                                    Question.update (Question.GetQuestionPage page) model.question model.global
                            in
                                ( { model | question = updatedQuestion, currentDrawerLinks = QuestionDefault }, Cmd.map QuestionMsg cmd )

                        QuestionTagSearchRoute page ->
                            let
                                ( updatedQuestion, cmd ) =
                                    Question.update (Question.GetQuestionPageSearch page) model.question model.global
                            in
                                ( { model | question = updatedQuestion, currentDrawerLinks = QuestionDefault }, Cmd.map QuestionMsg cmd )

                        UserQuestionListRoute page ->
                            let
                                ( updatedQuestion, cmd ) =
                                    Question.update (Question.GetMineQuestionListPage page) model.question model.global
                            in
                                ( { model | question = updatedQuestion, currentDrawerLinks = QuestionDefault }, Cmd.map QuestionMsg cmd )

                        SelectedQuestionListRoute page ->
                            let
                                ( updatedQuestion, cmd ) =
                                    Question.update (Question.QuestionListMsg <| QuestionList.GetQuestionList page) model.question model.global
                            in
                                ( { model | question = updatedQuestion, currentDrawerLinks = QuestionDefault }, Cmd.map QuestionMsg cmd )

                        VerifyEmailRoute emailKey ->
                            let
                                ( updatedKey, cmd ) =
                                    VerifyEmail.update (VerifyEmail.VerifyKey emailKey) model.verifyEmail
                            in
                                ( { model | verifyEmail = updatedKey, currentDrawerLinks = QuestionDefault }, Cmd.map VerifyEmailMsg cmd )

                        ResetPasswordRoute codUser key ->
                            let
                                ( updatedPassword, cmd ) =
                                    ResetPassword.update (ResetPassword.ResetPassword codUser key) model.resetPassword
                            in
                                ( { model | resetPassword = updatedPassword, currentDrawerLinks = QuestionDefault }, Cmd.map ResetPasswordMsg cmd )

                        ResetPasswordEmailRoute ->
                            let
                                ( updatedPassword, cmd ) =
                                    ResetPassword.update (ResetPassword.SendEmail) model.resetPassword
                            in
                                ( { model | resetPassword = updatedPassword, currentDrawerLinks = QuestionDefault }, Cmd.map ResetPasswordMsg cmd )

                        RedirectRoute redirectHash ->
                            { model | redirectHash = Just redirectHash } ! []

                        _ ->
                            ( model, Cmd.none )
            in
                ( { newModel | route = newRoute, currentDrawerLinks = newDrawerLink }, cmd )

        ShowIndex ->
            let
                global =
                    model.global
            in
                case global.user of
                    Nothing ->
                        ( { model | currentDrawerLinks = HomeDefault }, Navigation.newUrl "#index" )

                    Just user ->
                        ( { model | currentDrawerLinks = LoggedIn }, Navigation.newUrl "#index" )

        ShowLogin ->
            ( model, Navigation.newUrl "#login" )

        ShowSignup ->
            ( model, Navigation.newUrl "#signup" )

        ShowUser ->
            ( model, Navigation.newUrl "#users" )

        UpdateDrawerLinks newLinks ->
            ( model, Cmd.none )

        Mdl msg_ ->
            Material.update Mdl msg_ model


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
