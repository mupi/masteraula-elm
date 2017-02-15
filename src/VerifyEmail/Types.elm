module VerifyEmail.Types exposing (..)

import Http
import Material


type alias Model =
    { key : String
    , error : String
    , success : String
    , mdl : Material.Model
    }


type alias VerifyEmailModel =
    { detail : String
    }


type alias EmailKey =
    String


type Msg
    = VerifyKey String
    | VerifyEmail
    | OnFetchVerifyEmail (Result Http.Error VerifyEmailModel)
    | NoOp
    | Mdl (Material.Msg Msg)
