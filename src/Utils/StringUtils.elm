module Utils.StringUtils exposing (..)

import Regex exposing (..)
import Date exposing (..)


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


monthToInt : Month -> Int
monthToInt month =
    case month of
        Jan ->
            1

        Feb ->
            2

        Mar ->
            3

        Apr ->
            4

        May ->
            5

        Jun ->
            6

        Jul ->
            7

        Aug ->
            8

        Sep ->
            9

        Oct ->
            10

        Nov ->
            11

        Dec ->
            12


dateToString : Result String Date -> String
dateToString res =
    case res of
        Ok date ->
            String.concat [ toString <| day date, "/", toString <| monthToInt (month date), "/", toString <| year date ]

        Err error ->
            ""


timeToString : Result String Date -> String
timeToString res =
    case res of
        Ok date ->
            String.concat [ toString <| hour date, ":", toString <| minute date, ":", toString <| second date ]

        Err error ->
            ""


intToResponseString : Int -> String
intToResponseString index =
    case index of
        0 ->
            "a"

        1 ->
            "b"

        2 ->
            "c"

        3 ->
            "d"

        4 ->
            "e"

        _ ->
            ""


removeEnters : String -> String
removeEnters text =
    replace All (regex "<\\/?[bB][rR]>*>") (\_ -> "") <|
        replace All (regex "<\\/?[pP]>*>") (\_ -> "") <|
            replace All (regex "\x0D\n\t") (\_ -> "") text


validateEmail : String -> Bool
validateEmail email =
    let
        emailRegex =
            "^\\S+@\\S+\\.\\S+$"
    in
        if String.length (replace All (regex emailRegex) (\_ -> "") email) <= 0 then
            True
        else
            False
