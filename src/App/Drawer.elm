module App.Drawer exposing (..)

import App.Routing exposing (..)
import User.Types as User


type DrawerLinks
    = HomeDefault
    | LoggedIn
    | QuestionDefault
    | UsersView


parseDrawerLink : Maybe User.User -> Route -> DrawerLinks
parseDrawerLink user route =
    case user of
        Just user ->
            case route of
                UserRoute ->
                    UsersView

                QuestionsRoute _ ->
                    QuestionDefault

                _ ->
                    LoggedIn

        Nothing ->
            case route of
                UserRoute ->
                    UsersView

                LoginRoute ->
                    LoggedIn

                QuestionsRoute _ ->
                    QuestionDefault

                _ ->
                    HomeDefault
