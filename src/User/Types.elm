module User.Types exposing (..)


type alias Model =
    { id : Int
    , username : String
    , name : String
    , email : String
    }


type Msg
    = NoOp
