module User.Types exposing (..)

import Material
import Material.Snackbar as Snackbar
import Http


type alias Model =
    { user : User
    , editUser : User
    , otherUser : User
    , password : String
    , newPassword : String
    , confirmPassword : String
    , snackbar : Snackbar.Model Int
    , mdl : Material.Model
    }


type alias User =
    { id : Int
    , username : String
    , name : String
    , email : String
    }


type alias UserId =
    Int


type Msg
    = GetUser UserId
    | ProfileSee
    | ProfileEdit
    | ProfileUpdate
    | SetName String
    | SetEmail String
    | SetPassword String
    | SetNewPassword String
    | SetConfirmPassword String
    | PasswordChange
    | OnFetchPasswordChange (Result Http.Error String)
    | OnFetchProfileUpdate (Result Http.Error String)
    | OnFetchGetUser (Result Http.Error User)
    | NoOp
    | Snackbar (Snackbar.Msg Int)
    | Mdl (Material.Msg Msg)
