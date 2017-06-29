module Question.Rest exposing (..)

import Http


headerBuild : Maybe String -> List Http.Header
headerBuild token =
    case token of
        Just token ->
            [ Http.header "Authorization" (String.concat [ "JWT ", token ]) ]

        Nothing ->
            []
