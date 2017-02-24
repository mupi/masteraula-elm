module Utils.MDLUtils exposing (..)

import Html.Events exposing (..)
import Json.Decode as Json
import Material.Options as Options


onEnter : msg -> Options.Property c msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Json.succeed msg
            else
                Json.fail "not ENTER"
    in
        Options.on "keydown" (Json.andThen isEnter keyCode)
