module Utils.StringUtils exposing (..)

import Regex exposing (..)


removeAccents : String -> String
removeAccents text =
    let
        lowerText =
            String.toLower text

        removeA =
            replace All (regex "[áàãâ]") (\_ -> "a") lowerText

        removeE =
            replace All (regex "[éê]") (\_ -> "e") removeA

        removeI =
            replace All (regex "[í]") (\_ -> "i") removeE

        removeO =
            replace All (regex "[óõô]") (\_ -> "o") removeI

        removeU =
            replace All (regex "[ú]") (\_ -> "u") removeO
    in
        removeU


spaceToTrace : String -> String
spaceToTrace text =
    replace All (regex " ") (\_ -> "-") text


removeSpaces : String -> String
removeSpaces text =
    replace All (regex " ") (\_ -> "") text


removeSpecialCharacters : String -> String
removeSpecialCharacters text =
    replace All (regex "[^a-z\\-A-Z ]") (\_ -> "") text


tagFormatter : String -> String
tagFormatter tag =
    removeSpecialCharacters <| spaceToTrace <| removeAccents tag
