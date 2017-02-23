module App.Drawer exposing (..)

import App.Routing exposing (..)
import User.Types as User


type DrawerLinks
    = HomeDefault
    | LoggedIn
    | QuestionDefault
    | UsersView


parseDrawerLink : Maybe User.Model -> Route -> DrawerLinks
parseDrawerLink user route =
    case user of
        Just user ->
            case route of
                UsersRoute ->
                    UsersView

                QuestionRoute a ->
                    QuestionDefault

                QuestionPageRoute a ->
                    QuestionDefault

                QuestionListRoute ->
                    QuestionDefault

                QuestionTagSearchRoute a ->
                    QuestionDefault

                _ ->
                    LoggedIn

        Nothing ->
            case route of
                UsersRoute ->
                    UsersView

                LoginRoute ->
                    LoggedIn

                QuestionRoute a ->
                    QuestionDefault

                QuestionPageRoute a ->
                    QuestionDefault

                QuestionListRoute ->
                    QuestionDefault

                QuestionTagSearchRoute a ->
                    QuestionDefault

                _ ->
                    HomeDefault
