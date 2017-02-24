module Main exposing (..)

import App.View exposing (view)
import App.State exposing (init, update, subscriptions)
import App.Types exposing (Model, Msg(..), LocalStorage)
import Navigation exposing (programWithFlags)


main : Program (Maybe LocalStorage) Model Msg
main =
    programWithFlags OnLocationChange
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
