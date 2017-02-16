module App.Types exposing (..)

import Navigation exposing (Location)
import App.Routing exposing (Route)
import Login.Types as Login
import Signup.Types as Signup
import VerifyEmail.Types as VerifyEmail
import Question.Types as Question
import User.Types as User
import Material


type alias Model =
    { login : Login.Model
    , signup : Signup.Model
    , verifyEmail : VerifyEmail.Model
    , question : Question.Model
    , route : Route
    , global : Global
    , mdl : Material.Model
    }


type alias GlobalStorage =
    { user : Maybe User.Model
    , token : Maybe String
    }


type alias Global =
    { user : Maybe User.Model
    , token : Maybe String
    }


type Status
    = Login


type Msg
    = UserMsg User.Msg
    | LoginMsg Login.Msg
    | SignupMsg Signup.Msg
    | VerifyEmailMsg VerifyEmail.Msg
    | QuestionMsg Question.Msg
    | OnLocationChange Location
    | ShowIndex
    | ShowLogin
    | ShowSignup
    | ShowUser
    | Mdl (Material.Msg Msg)
