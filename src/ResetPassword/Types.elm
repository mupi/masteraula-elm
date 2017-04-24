module ResetPassword.Types exposing (..)

import Http
import Material
import Material.Snackbar as Snackbar
import Login.Types as Login


type alias Model =
    { email : String
    , codUser : String
    , key : String
    , newPassword : String
    , confirmPassword : String
    , reseting : Bool
    , error : String
    , success : Bool
    , snackbar : Snackbar.Model Int
    , mdl : Material.Model
    }


type alias EmailKey =
    String


type Msg
    = SetEmail String
    | SendEmail
    | ResetPassword String String
    | SetNewPassword String
    | SetConfirmationPassword String
    | Reset
    | Send
    | OnFetchSendEmail (Result Http.Error String)
    | OnFetchResetPassword (Result Http.Error String)
    | NoOp
    | Snackbar (Snackbar.Msg Int)
    | Mdl (Material.Msg Msg)
