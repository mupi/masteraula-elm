module Question.Question.State exposing (init, update, initQuestion, questionParent)

import Http
import Navigation
import Material


-- My modules

import App.Types as App
import Question.Question.Types exposing (..)
import Question.Question.Rest exposing (..)
import User.State as User


initQuestion : Question
initQuestion =
    Question
        0
        (QuestionParent Nothing)
        ""
        Nothing
        User.initUser
        0
        []
        Nothing
        []
        []
        Nothing
        Nothing
        Nothing
        []
        (RelatedQuestion [])


init : Model
init =
    Model
        initQuestion
        ""
        Material.model


questionParent : Question -> Maybe Question
questionParent question =
    case question.question_parent of
        QuestionParent p ->
            p


update : Msg -> Model -> App.Global -> ( Model, Cmd Msg )
update msg model global =
    case msg of
        GetQuestion questionId ->
            model ! [ fetchGetQuestion questionId global.token ]

        OnFetchGetQuestion (Ok question) ->
            { model | question = question, error = "" } ! []

        OnFetchGetQuestion (Err error) ->
            let
                errorMsg =
                    case error of
                        Http.NetworkError ->
                            "Erro de conexão com o servidor"

                        Http.BadStatus response ->
                            "Questão inexistente"

                        _ ->
                            toString error
            in
                { model | error = errorMsg } ! []

        Mdl msg_ ->
            Material.update Mdl msg_ model
