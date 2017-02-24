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


initQuestionListPage : QuestionListPage
initQuestionListPage =
    QuestionListPage 0 1 Nothing Nothing []


initQuestionList : QuestionList
initQuestionList =
    QuestionList 0 "" False User.init [] ""


init : Model
init =
    Model
        initQuestion
        initQuestionPage
        initQuestionList
        initQuestionList
        initQuestionListPage
        ""
        []
        0
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

        GetMineQuestionListPage questionPage ->
            { model | tags = [] } ! [ fetchGetMineQuestionList questionPage global.token ]

        GetQuestionList questionListId ->
            { model | tags = [] } ! [ fetchGetQuestionList questionListId global.token ]

        GetQuestionTagSearch questionPage ->
            model ! [ fetchGetQuestionFilterSearch questionPage model.tags model.filterId global.token ]

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
                { model | tags = tags, currentTag = "" } ! [ Navigation.newUrl "#questions/tagsearch/1" ]

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
                { model | tags = tags } ! [ Navigation.newUrl "#questions/tagsearch/1" ]

        QuestionListAdd question ->
            let
                questionList =
                    model.questionListEdit

                updQuestionList =
                    { questionList | questions = QuestionOrder question 0 :: questionList.questions }

                newQuestionList =
                    { questionList
                        | questions = List.indexedMap (\index questionOrder -> { questionOrder | order = index }) updQuestionList.questions
                    }
            in
                { model | questionListEdit = newQuestionList } ! []

        QuestionListRemove question ->
            let
                questionList =
                    model.questionListEdit

                newQuestions =
                    List.filterMap
                        (\questionOrder ->
                            if questionOrder.question == question then
                                Nothing
                            else
                                Just questionOrder
                        )
                        questionList.questions

                newQuestionList =
                    { questionList
                        | questions = List.indexedMap (\index questionOrder -> { questionOrder | order = index }) newQuestions
                    }
            in
                { model | questionListEdit = newQuestionList } ! []

        QuestionListHeaderInput newQuestionHeader ->
            let
                questionList =
                    model.questionListEdit

                newQuestionList =
                    { questionList | question_list_header = newQuestionHeader }
            in
                { model | questionListEdit = newQuestionList } ! []

        QuestionListSave ->
            model ! [ fetchPostSaveQuestionList model.questionListEdit global.token ]

        QuestionListClear ->
            { model | questionListEdit = initQuestionList } ! []

        QuestionListGenerate questionList ->
            if (model.questionListEdit == questionList) then
                { model | generateAfterSave = True } ! [ fetchPostSaveQuestionList questionList global.token ]
            else
                model ! [ fetchGetGenerateList questionList.id global.token ]

        QuestionListDelete ->
            model ! [ fetchDeleteQuestionList model.questionListEdit global.token ]

        QuestionListClick questionListId ->
            model ! [ Navigation.newUrl <| String.concat [ "#questions/questionlists/", toString questionListId ] ]

        Filter newFilterId ->
            { model | filterId = newFilterId } ! [ fetchGetQuestionFilterSearch 1 model.tags newFilterId global.token ]

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

        OnFetchGetQuestionFilterSearch (Ok questionPage) ->
            { model | questionPage = questionPage, error = "" } ! []

        OnFetchGetQuestionFilterSearch (Err error) ->
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
                    model.questionListEdit

                newQuestionList =
                    if model.generateAfterSave then
                        { questionList | id = newId }
                    else
                        initQuestionList

                cmds =
                    if model.generateAfterSave then
                        [ fetchGetGenerateList newId global.token ]
                    else
                        [ Navigation.newUrl <| String.concat [ "#questions/questionlists/", toString newId ] ]
            in
                { model | questionListEdit = newQuestionList, generateAfterSave = False } ! cmds

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
            { model | questionListEdit = initQuestionList } ! []

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

        OnFetchGetMineQuestionListPage (Ok questionListPage) ->
            { model | questionListPage = questionListPage, error = "" } ! []

        OnFetchGetMineQuestionListPage (Err error) ->
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

        OnFetchGetQuestionList (Ok questionList) ->
            { model | questionListSelected = questionList, error = "" } ! []

        OnFetchGetQuestionList (Err error) ->
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
