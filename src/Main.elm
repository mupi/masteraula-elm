module Main exposing (..)

import App.View exposing (view)
import App.State exposing (init, update, subscriptions)
import App.Types exposing (Model, Msg(..), GlobalStorage)
import Navigation exposing (programWithFlags)


main : Program (Maybe GlobalStorage) Model Msg
main =
    programWithFlags OnLocationChange
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
