module ResetPassword.Types exposing (..)

import Http
import Material
import Material.Snackbar as Snackbar
import Login.Types as Login


type alias Model =
    { codUser : String
    , key : String
    , newPassword : String
    , confirmPassword : String
    , error : String
    , success : String
    , login : Login.Model
    , snackbar : Snackbar.Model Int
    , mdl : Material.Model
    }


type alias EmailKey =
    String


type Msg
    = ResetPassword String String
    | SetNewPassword String
    | SetConfirmationPassword String
    | Reset
    | OnFetchResetPassword (Result Http.Error String)
    | NoOp
    | LoginMsg Login.Msg
    | Snackbar (Snackbar.Msg Int)
    | Mdl (Material.Msg Msg)
