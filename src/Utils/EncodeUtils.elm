module Utils.EncodeUtils exposing (..)

import Json.Encode exposing (..)


maybeString : Maybe String -> Value
maybeString text =
    case text of
        Just t ->
            string t

        Nothing ->
            null


maybeInt : Maybe Int -> Value
maybeInt integer =
    case integer of
        Just i ->
            int i

        Nothing ->
            null
