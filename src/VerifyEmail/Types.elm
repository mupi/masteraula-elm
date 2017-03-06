module VerifyEmail.Types exposing (..)

import Http
import Material
import Material.Snackbar as Snackbar
import Login.Types as Login


type alias Model =
    { key : String
    , error : String
    , success : String
    , login : Login.Model
    , snackbar : Snackbar.Model Int
    , mdl : Material.Model
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
    | LoginMsg Login.Msg
    | Snackbar (Snackbar.Msg Int)
    | Mdl (Material.Msg Msg)
