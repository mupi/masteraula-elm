port module App.State exposing (init, update, subscriptions)

import Navigation exposing (Location)
import App.Routing as Routing
import App.Drawer exposing (..)
import App.Routing exposing (parseLocation, Route(..))
import App.Types exposing (..)
import Login.State as Login
import Login.Types as Login
import User.State as User
import User.Types as User
import Signup.State as Signup
import VerifyEmail.State as VerifyEmail
import VerifyEmail.Types as VerifyEmail
import Question.State as Question
import Question.Types as Question
import User.State as User
import Material


port setLocalStorage : LocalStorage -> Cmd msg


port displayDialog : String -> Cmd msg


init : Maybe LocalStorage -> Location -> ( Model, Cmd Msg )
init savedStorage location =
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
                (initQuestion savedStorage)
                (initUser savedStorage)
                currentRoute
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
            Question.init
            User.init
            Routing.IndexRoute
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
    Maybe.withDefault (LocalStorage Nothing Nothing Question.initQuestionList) savedStorage


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
                ( updatedLogin, cmd ) =
                    Login.update subMsg model.login

                newCmd =
                    Cmd.map LoginMsg cmd
            in
                if subMsg == Login.Logout then
                    ( initClear, Cmd.batch [ setLocalStorage (initStorage Nothing), newCmd ] )
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
                        ( { model | login = updatedLogin, user = newUser, global = newGlobal, localStorage = newStorage }, Cmd.batch [ setLocalStorage newStorage, newCmd ] )

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
                            ++ case subMsg of
                                Question.Dialog a ->
                                    [ displayDialog "elm-mdl-singleton-dialog" ]

                                _ ->
                                    []
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
                                    Question.update (Question.GetQuestion questionId) model.question model.global
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
                                    Question.update (Question.GetQuestionTagSearch page) model.question model.global
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
                                    Question.update (Question.GetQuestionList page) model.question model.global
                            in
                                ( { model | question = updatedQuestion, currentDrawerLinks = QuestionDefault }, Cmd.map QuestionMsg cmd )

                        VerifyEmailRoute emailKey ->
                            let
                                ( updatedKey, cmd ) =
                                    VerifyEmail.update (VerifyEmail.VerifyKey emailKey) model.verifyEmail
                            in
                                ( { model | verifyEmail = updatedKey, currentDrawerLinks = QuestionDefault }, Cmd.map VerifyEmailMsg cmd )

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
