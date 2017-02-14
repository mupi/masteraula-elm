module Signup.Types exposing (..)

import Http
import User.Types as User
import Material


type alias Model =
    { username : String
    , password : String
    , confirmPassword : String
    , email : String
    , error : String
    , success : Bool
    , mdl : Material.Model
    }


type alias RestErrorModel =
    { email : List String
    , username : List String
    , password1 : List String
    , password2 : List String
    , non_field_errors : List String
    }


type Msg
    = SetUsername String
    | SetPassword String
    | SetConfirmPassword String
    | SetEmail String
    | Signup
    | OnFetchSignup (Result Http.Error RestErrorModel)
    | NoOp
    | Mdl (Material.Msg Msg)
