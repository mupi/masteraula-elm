module App.Routing exposing (..)

import Navigation exposing (Location)
import UrlParser exposing (..)
import Question.Types as Question
import VerifyEmail.Types as VerifyEmail


type Route
    = IndexRoute
    | UsersRoute
    | LoginRoute
    | SignupRoute
    | VerifyEmailRoute VerifyEmail.EmailKey
    | QuestionRoute Question.QuestionId
    | QuestionPageRoute Question.PageNumber
    | QuestionListRoute
    | QuestionTagSearchRoute Question.PageNumber
    | NotFoundRote


normalMatchers : Parser (Route -> a) a
normalMatchers =
    oneOf
        [ map IndexRoute top
        , map IndexRoute (s "index")
        , map LoginRoute (s "login")
        , map SignupRoute (s "signup")
        , map VerifyEmailRoute (s "verify-email" </> string)
        , map QuestionPageRoute (s "questions" </> int)
        , map QuestionListRoute (s "questions" </> s "questionlist")
        , map QuestionRoute (s "question" </> int)
        , map QuestionTagSearchRoute (s "questions" </> s "tagsearch" </> int)
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
        , map QuestionTagSearchRoute (s "questions" </> s "tagsearch" </> int)
        , map UsersRoute (s "users")
        ]


parseLocation : Maybe a -> Location -> Route
parseLocation user location =
    case user of
        Just user ->
            case (parseHash loginMatchers location) of
                Just route ->
                    route

                Nothing ->
                    NotFoundRote

        Nothing ->
            case (parseHash normalMatchers location) of
                Just route ->
                    route

                Nothing ->
                    NotFoundRote
