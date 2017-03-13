module Login.Types exposing (..)

import Http
import User.Types as User
import Material


type alias Model =
    { user : Maybe User.User
    , username : String
    , password : String
    , token : Maybe String
    , error : String
    , mdl : Material.Model
    }


type alias LoginModel =
    { user : User.User
    , token : String
    }


type Msg
    = SetUsername String
    | SetPassword String
    | Login
    | Logout
    | OnFetchLogin (Result Http.Error LoginModel)
    | NoOp
    | Mdl (Material.Msg Msg)
