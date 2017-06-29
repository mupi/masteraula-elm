port module App.Ports exposing (displayDialog, setLocalStorage)

import Json.Encode as Encode


-- My Modules

import App.Types exposing (..)
import App.Rest exposing (..)


port displayDialog : String -> Cmd msg


port portLocalStorage : Encode.Value -> Cmd msg


setLocalStorage : LocalStorage -> Cmd msg
setLocalStorage localStorage =
    portLocalStorage <| localStorageEncoder localStorage
