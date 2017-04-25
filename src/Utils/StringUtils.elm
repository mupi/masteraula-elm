module Utils.StringUtils exposing (..)

import Regex exposing (..)
import Date exposing (..)


removeAccents : String -> String
removeAccents text =
    let
        removeA =
            replace All (regex "[ÁÀÃÂ]") (\_ -> "A") text

        removeE =
            replace All (regex "[ÉÊ]") (\_ -> "E") removeA

        removeI =
            replace All (regex "[Í]") (\_ -> "I") removeE

        removeO =
            replace All (regex "[ÓÕÔ]") (\_ -> "O") removeI

        removeU =
            replace All (regex "[Ú]") (\_ -> "U") removeO

        removea =
            replace All (regex "[áàãâ]") (\_ -> "a") removeU

        removee =
            replace All (regex "[éê]") (\_ -> "e") removea

        removei =
            replace All (regex "[í]") (\_ -> "i") removee

        removeo =
            replace All (regex "[óõô]") (\_ -> "o") removei

        removeu =
            replace All (regex "[ú]") (\_ -> "u") removeo
    in
        removeu


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
    removeSpecialCharacters <| spaceToTrace <| removeAccents <| String.toLower tag


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


maybeStringToString : Maybe String -> String
maybeStringToString text =
    case text of
        Just t ->
            t

        Nothing ->
            ""


maybeIntToString : Maybe Int -> String
maybeIntToString number =
    case number of
        Just t ->
            toString t

        Nothing ->
            ""


hasNoSpecialCharacters : String -> Bool
hasNoSpecialCharacters text =
    let
        newText =
            replace All (regex "[^a-z\\A-Z 0-9]") (\_ -> "") <| removeAccents text
    in
        text == newText
