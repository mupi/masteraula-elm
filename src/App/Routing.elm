module App.Routing exposing (..)

import Navigation exposing (Location)
import UrlParser exposing (..)
import Question.Types as Question
import VerifyEmail.Types as VerifyEmail
import User.Types as User


type Route
    = IndexRoute
    | UserRoute
    | UserOtherRoute Int
    | UserUpdateRoute
    | LoginRoute
    | SignupRoute
    | VerifyEmailRoute VerifyEmail.EmailKey
    | ResetPasswordRoute String String
    | QuestionRoute Question.QuestionId
    | QuestionPageRoute Question.PageNumber
    | QuestionListRoute
    | SelectedQuestionListRoute Question.QuestionId
    | UserQuestionListRoute Question.PageNumber
    | QuestionTagSearchRoute Question.PageNumber
    | RedirectRouteAux Route
    | RedirectRoute String
    | NotFoundRoute


normalMatchers : Parser (Route -> a) a
normalMatchers =
    oneOf
        [ map IndexRoute top
        , map IndexRoute (s "index")
        , map LoginRoute (s "login")
        , map SignupRoute (s "signup")
        , map VerifyEmailRoute (s "verify-email" </> string)
        , map ResetPasswordRoute (s "reset-password" </> string </> string)
        , map RedirectRouteAux <| map QuestionPageRoute (s "questions" </> int)
        , map RedirectRouteAux <| map QuestionRoute (s "question" </> int)
        , map RedirectRouteAux <| map QuestionListRoute (s "questions" </> s "questionlist")
        , map RedirectRouteAux <| map SelectedQuestionListRoute (s "questions" </> s "questionlists" </> int)
        , map RedirectRouteAux <| map UserQuestionListRoute (s "questions" </> s "user_lists" </> int)
        , map RedirectRouteAux <| map QuestionTagSearchRoute (s "questions" </> s "tagsearch" </> int)
        , map RedirectRouteAux <| map UserOtherRoute (s "users" </> int)
        , map RedirectRouteAux <| map UserRoute (s "users")
        , map RedirectRouteAux <| map UserUpdateRoute (s "users" </> s "updateprofile")
        ]


loginMatchers : Parser (Route -> a) a
loginMatchers =
    oneOf
        [ map IndexRoute top
        , map IndexRoute (s "index")
        , map IndexRoute (s "login")
        , map IndexRoute (s "signup")
        , map IndexRoute (s "verify-email")
        , map QuestionPageRoute (s "questions" </> int)
        , map QuestionRoute (s "question" </> int)
        , map QuestionListRoute (s "questions" </> s "questionlist")
        , map SelectedQuestionListRoute (s "questions" </> s "questionlists" </> int)
        , map UserQuestionListRoute (s "questions" </> s "user_lists" </> int)
        , map QuestionTagSearchRoute (s "questions" </> s "tagsearch" </> int)
        , map UserOtherRoute (s "users" </> int)
        , map UserRoute (s "users")
        , map UserUpdateRoute (s "users" </> s "updateprofile")
        ]


parseLocation : Maybe User.User -> Location -> Route
parseLocation user location =
    case user of
        Just user ->
            case (parseHash loginMatchers location) of
                Just route ->
                    route

                Nothing ->
                    NotFoundRoute

        Nothing ->
            case (parseHash normalMatchers location) of
                Just route ->
                    case route of
                        RedirectRouteAux route ->
                            RedirectRoute location.hash

                        _ ->
                            route

                Nothing ->
                    NotFoundRoute
