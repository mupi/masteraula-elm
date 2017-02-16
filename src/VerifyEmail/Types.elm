module VerifyEmail.Types exposing (..)

import Http
import Material
import Login.Types as Login


type alias Model =
    { key : String
    , error : String
    , success : String
    , mdl : Material.Model
    , login : Login.Model
    }


type alias VerifyEmailModel =
    { detail : String
    }


type alias EmailKey =
    String


type Msg
    = VerifyKey String
    | OnFetchVerifyEmail (Result Http.Error VerifyEmailModel)
    | NoOp
    | Mdl (Material.Msg Msg)
    | LoginMsg Login.Msg
