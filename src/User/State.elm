module User.State exposing (init, initUser, update)

import User.Types exposing (..)


init : Model
init =
    Model initUser


initUser : User
initUser =
    User 0 "" "" ""


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []
