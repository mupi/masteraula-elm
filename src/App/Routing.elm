module App.Routing exposing (..)

import Navigation exposing (Location)
import UrlParser exposing (..)


-- My modules

import Question.Routing as Question
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
    | ResetPasswordEmailRoute
    | QuestionsRoute Question.Route
      -- | QuestionRoute Question.QuestionId
      -- | QuestionPageRoute Int
      -- | QuestionListRoute
      -- | SelectedQuestionListRoute Question.QuestionId
      -- | UserQuestionListRoute Int
      -- | QuestionTagSearchRoute Int
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
        , map ResetPasswordEmailRoute (s "reset-password")
        , map RedirectRouteAux <| map QuestionsRoute <| map Question.QuestionPageRoute (s "questions" </> int)
        , map RedirectRouteAux <| map QuestionsRoute <| map Question.QuestionRoute (s "question" </> int)
        , map RedirectRouteAux <| map QuestionsRoute <| map Question.QuestionListRoute (s "questions" </> s "questionlist")
        , map RedirectRouteAux <| map QuestionsRoute <| map Question.SelectedQuestionListRoute (s "questions" </> s "questionlists" </> int)
        , map RedirectRouteAux <| map QuestionsRoute <| map Question.MineQuestionListsRoute (s "questions" </> s "user_lists")
        , map RedirectRouteAux <| map QuestionsRoute <| map Question.QuestionTagSearchRoute (s "questions" </> s "tagsearch" </> int)
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
        , map QuestionsRoute <| map Question.QuestionPageRoute (s "questions" </> int)
        , map QuestionsRoute <| map Question.QuestionRoute (s "question" </> int)
        , map QuestionsRoute <| map Question.QuestionListRoute (s "questions" </> s "questionlist")
        , map QuestionsRoute <| map Question.SelectedQuestionListRoute (s "questions" </> s "questionlists" </> int)
        , map QuestionsRoute <| map Question.MineQuestionListsRoute (s "questions" </> s "user_lists")
        , map QuestionsRoute <| map Question.QuestionTagSearchRoute (s "questions" </> s "tagsearch" </> int)
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
