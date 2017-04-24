module App.Types exposing (..)

import Navigation exposing (Location)
import App.Routing exposing (Route)
import App.Drawer exposing (DrawerLinks)
import Login.Types as Login
import Signup.Types as Signup
import VerifyEmail.Types as VerifyEmail
import ResetPassword.Types as ResetPassword
import Question.Types as Question
import User.Types as User
import Material


type alias Model =
    { login : Login.Model
    , signup : Signup.Model
    , verifyEmail : VerifyEmail.Model
    , resetPassword : ResetPassword.Model
    , question : Question.Model
    , user : User.Model
    , route : Route
    , redirectHash : Maybe String
    , global : Global
    , localStorage : LocalStorage
    , currentDrawerLinks : DrawerLinks
    , mdl : Material.Model
    }


type alias LocalStorage =
    { user : Maybe User.User
    , token : Maybe String
    , questionList : Question.QuestionList
    }


type alias Global =
    { user : Maybe User.User
    , token : Maybe String
    }


type Status
    = Login


type Msg
    = UserMsg User.Msg
    | LoginMsg Login.Msg
    | SignupMsg Signup.Msg
    | VerifyEmailMsg VerifyEmail.Msg
    | ResetPasswordMsg ResetPassword.Msg
    | QuestionMsg Question.Msg
    | OnLocationChange Location
    | ShowIndex
    | ShowLogin
    | ShowSignup
    | ShowUser
    | UpdateDrawerLinks DrawerLinks
    | Mdl (Material.Msg Msg)
