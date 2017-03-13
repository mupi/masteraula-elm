module User.Types exposing (..)


type alias Model =
    { user : User }


type alias User =
    { id : Int
    , username : String
    , name : String
    , email : String
    }


type Msg
    = NoOp
