module Question.State exposing (init, update)

import Http
import Question.Types exposing (..)
import Question.Rest exposing (fetchGetQuestion, fetchGetQuestionPage, fetchGetQuestionTagSearch)
import App.Types as App
import Navigation
import Material
import Utils.StringUtils as StringUtils


initQuestion : Question
initQuestion =
    Question 0 "" "" Nothing "" 0 "" [] []


initQuestionPage : QuestionPage
initQuestionPage =
    QuestionPage 0 1 Nothing Nothing []


init : Model
init =
    Model
        initQuestion
        initQuestionPage
        ""
        []
        ""
        Material.model


update : Msg -> Model -> App.Global -> ( Model, Cmd Msg )
update msg model global =
    case msg of
        GetQuestion questionId ->
            model ! [ fetchGetQuestion questionId global.token ]

        GetQuestionPage questionPage ->
            { model | tags = [] } ! [ fetchGetQuestionPage questionPage global.token ]

        GetQuestionTagSearch questionPage ->
            model ! [ fetchGetQuestionTagSearch questionPage model.tags global.token ]

        ChangePage page ->
            if model.tags == [] then
                model ! [ Navigation.newUrl <| String.concat [ "#questions/", toString page ] ]
            else
                model ! [ Navigation.newUrl <| String.concat [ "#questions/tagsearch/", toString page ] ]

        TagSearchInput newCurrentTag ->
            { model | currentTag = newCurrentTag } ! []

        TagSearch ->
            let
                tags =
                    if (StringUtils.removeSpaces model.currentTag) == "" then
                        model.tags
                    else
                        model.currentTag :: model.tags
            in
                { model | tags = tags, currentTag = "" } ! [ Navigation.newUrl "#questions/tagsearch/1" ]

        TagSearchAdd ->
            let
                tags =
                    if (StringUtils.removeSpaces model.currentTag) == "" then
                        model.tags
                    else
                        model.currentTag :: model.tags
            in
                { model | tags = tags, currentTag = "" } ! []

        TagSearchRemove tag ->
            let
                tags =
                    List.filterMap
                        (\t ->
                            if t == tag then
                                Nothing
                            else
                                Just t
                        )
                        model.tags
            in
                { model | tags = tags } ! []

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

        OnFetchGetQuestionPage (Ok questionPage) ->
            { model | questionPage = questionPage, error = "" } ! []

        OnFetchGetQuestionPage (Err error) ->
            let
                errorMsg =
                    case error of
                        Http.NetworkError ->
                            "Erro de conexão com o servidor"

                        Http.BadStatus response ->
                            "Página inexistente"

                        _ ->
                            toString error
            in
                { model | error = errorMsg } ! []

        OnFetchGetQuestionTagSearch (Ok questionPage) ->
            { model | questionPage = questionPage, error = "" } ! []

        OnFetchGetQuestionTagSearch (Err error) ->
            let
                errorMsg =
                    case error of
                        Http.NetworkError ->
                            "Erro de conexão com o servidor"

                        Http.BadStatus response ->
                            "Página inexistente"

                        _ ->
                            toString error
            in
                { model | error = errorMsg } ! []

        NoOp ->
            model ! []

        Mdl msg_ ->
            Material.update Mdl msg_ model
