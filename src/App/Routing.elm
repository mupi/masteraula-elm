module App.Routing exposing (..)

import Navigation exposing (Location)
import UrlParser exposing (..)
import User.Types as User
import Question.Types as Question


type Route
    = Index
    | UsersRoute
    | LoginRoute
    | QuestionRoute Question.QuestionId
    | QuestionPageRoute Question.PageNumber
    | QuestionTagSearchRoute Question.PageNumber
    | NotFoundRote


normalMatchers : Parser (Route -> a) a
normalMatchers =
    oneOf
        [ map Index top
        , map Index (s "index")
        , map LoginRoute (s "login")
        , map QuestionPageRoute (s "questions" </> int)
        , map QuestionRoute (s "question" </> int)
        , map QuestionTagSearchRoute (s "questions" </> s "tagsearch" </> int)
        ]


loginMatchers : Parser (Route -> a) a
loginMatchers =
    oneOf
        [ map Index top
        , map Index (s "index")
        , map Index (s "login")
        , map QuestionPageRoute (s "questions" </> int)
        , map QuestionRoute (s "question" </> int)
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
