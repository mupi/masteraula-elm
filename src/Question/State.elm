module Question.State exposing (init, update, initQuestionList)

import Http
import Question.Types exposing (..)
import Question.Rest exposing (..)
import App.Types as App
import User.State as User
import Navigation
import Material
import Utils.StringUtils as StringUtils


initQuestion : Question
initQuestion =
    Question 0 "" "" Nothing User.init 0 [] []


initQuestionPage : QuestionPage
initQuestionPage =
    QuestionPage 0 1 Nothing Nothing []


initQuestionList : QuestionList
initQuestionList =
    QuestionList 0 "" False "" []


init : Model
init =
    Model
        initQuestion
        initQuestionPage
        initQuestionList
        ""
        []
        False
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

        QuestionListAdd question ->
            let
                questionList =
                    model.questionList

                newQuestionList =
                    { questionList | questions = question :: questionList.questions }
            in
                { model | questionList = newQuestionList } ! []

        QuestionListRemove question ->
            let
                questionList =
                    model.questionList

                newQuestions =
                    List.filterMap
                        (\t ->
                            if t == question then
                                Nothing
                            else
                                Just t
                        )
                        questionList.questions

                newQuestionList =
                    { questionList | questions = newQuestions }
            in
                { model | questionList = newQuestionList } ! []

        QuestionListHeaderInput newQuestionHeader ->
            let
                questionList =
                    model.questionList

                newQuestionList =
                    { questionList | question_list_header = newQuestionHeader }
            in
                { model | questionList = newQuestionList } ! []

        QuestionListSave ->
            model ! [ fetchPostSaveQuestioList model.questionList global.token ]

        QuestionListClear ->
            { model | questionList = initQuestionList } ! []

        QuestionListGenerate ->
            { model | generateAfterSave = True } ! [ fetchPostSaveQuestioList model.questionList global.token ]

        QuestionListDelete ->
            model ! [ fetchDeleteQuestioList model.questionList global.token ]

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

        OnFecthQuestionListGenerate (Ok id) ->
            let
                newUrl =
                    String.concat [ "http://localhost:8000/rest/question_lists/", id, "/get_list/" ]
            in
                { model | error = "" } ! [ Navigation.load newUrl ]

        OnFecthQuestionListGenerate (Err error) ->
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

        OnFetchSaveQuestionList (Ok newId) ->
            let
                questionList =
                    model.questionList

                newQuestionList =
                    { questionList | id = newId }

                cmds =
                    if model.generateAfterSave then
                        [ fetchGetGenerateList newId global.token ]
                    else
                        []
            in
                { model | questionList = newQuestionList, generateAfterSave = False } ! cmds

        OnFetchSaveQuestionList (Err error) ->
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

        OnFetchDeleteQuestionList (Ok text) ->
            { model | questionList = initQuestionList } ! []

        OnFetchDeleteQuestionList (Err error) ->
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
