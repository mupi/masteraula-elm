module User.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (id, type_, for, value, class)
import Html.Events exposing (..)
import User.Types exposing (..)


-- import State exposing (..)


view : Model -> Html Msg
view model =
    let
        user =
            model.user
    in
        div [ id "signup-form" ]
            [ Html.h1 [] [ text "User Details" ]
            , p [] [ text user.username ]
            , p [] [ text user.name ]
            , p [] [ text user.email ]
            , text (toString model)
            ]
