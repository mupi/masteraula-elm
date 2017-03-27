module Question.State exposing (init, update, initQuestionList)

import App.Config as Config
import Http
import Question.Types exposing (..)
import Question.Rest exposing (..)
import App.Types as App
import User.State as User
import Navigation
import Material
import Material.Snackbar as Snackbar
import Material.Helpers exposing (map1st, map2nd)
import Utils.StringUtils as StringUtils


initQuestion : Question
initQuestion =
    Question 0 "" Nothing User.initUser 0 [] "" [] [] Nothing Nothing Nothing


initQuestionPage : QuestionPage
initQuestionPage =
    QuestionPage 0 1 Nothing Nothing []


initQuestionListPage : QuestionListPage
initQuestionListPage =
    QuestionListPage 0 1 Nothing Nothing []


initQuestionList : QuestionList
initQuestionList =
    QuestionList 0 "" False User.initUser [] ""


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
        Delete
        Snackbar.model
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
            if model.tags == [] && model.filterId == 0 then
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
                newTags =
                    List.filterMap
                        (\t ->
                            if t == tag then
                                Nothing
                            else
                                Just t
                        )
                        model.tags
            in
                if newTags == [] && model.filterId == 0 then
                    { model | tags = newTags } ! [ Navigation.newUrl <| "#questions/1" ]
                else
                    { model | tags = newTags } ! [ Navigation.newUrl <| "#questions/tagsearch/1" ]

        QuestionClick question ->
            model ! [ Navigation.newUrl <| String.concat [ "#question/", toString question.id ] ]

        QuestionBack ->
            model ! [ Navigation.back 1 ]

        QuestionListAdd question ->
            let
                questionList =
                    model.questionListEdit

                updQuestionList =
                    { questionList | questions = List.append questionList.questions [ QuestionOrder question 0 ] }

                newQuestionList =
                    { questionList | questions = List.indexedMap (\index questionOrder -> { questionOrder | order = index + 1 }) updQuestionList.questions }
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
            if model.questionListEdit.question_list_header == "" then
                let
                    ( snackbar, effect ) =
                        Snackbar.add (Snackbar.snackbar 0 "Por favor, coloque o nome da lista anstes de salvá-la" "Fechar") model.snackbar
                            |> map2nd (Cmd.map Snackbar)
                in
                    { model | snackbar = snackbar } ! [ effect ]
            else
                { model | generateAfterSave = False } ! [ fetchPostSaveQuestionList model.questionListEdit global.token ]

        QuestionListClear ->
            let
                questionListEdit =
                    model.questionListEdit

                newQuestionList =
                    { questionListEdit | questions = [] }
            in
                { model | questionListEdit = newQuestionList } ! []

        QuestionListGenerate questionList ->
            model ! [ fetchGetGenerateList questionList.id global.token ]

        QuestionListDelete ->
            model ! [ fetchDeleteQuestionList model.questionListEdit global.token ]

        QuestionListClick questionListId ->
            model ! [ Navigation.newUrl <| String.concat [ "#questions/questionlists/", toString questionListId ] ]

        QuestionListEdit questionList ->
            { model | questionListEdit = questionList } ! [ Navigation.newUrl <| String.concat [ "#questions/questionlist" ] ]

        QuestionListCancel ->
            let
                questionListId =
                    model.questionListEdit.id
            in
                { model | questionListEdit = initQuestionList } ! [ Navigation.newUrl <| String.concat [ "#questions/questionlists/", toString questionListId ] ]

        Filter newFilterId ->
            if model.tags == [] && newFilterId == 0 then
                { model | filterId = newFilterId } ! [ Navigation.newUrl <| "#questions/1" ]
            else
                { model | filterId = newFilterId } ! [ Navigation.newUrl <| "#questions/tagsearch/1" ]

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
                    String.concat [ Config.baseUrl, "question_lists/", id, "/get_list/" ]
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

                ( snackbar, effect ) =
                    Snackbar.add (Snackbar.snackbar 0 errorMsg "Close") model.snackbar
                        |> map2nd (Cmd.map Snackbar)
            in
                { model | error = errorMsg, snackbar = snackbar } ! [ effect ]

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

        Dialog dialog ->
            { model | dialog = dialog } ! []

        NoOp ->
            model ! []

        Snackbar (Snackbar.End a) ->
            { model | snackbar = Snackbar.model } ! []

        Snackbar msg ->
            Snackbar.update msg model.snackbar
                |> map1st (\s -> { model | snackbar = s })
                |> map2nd (Cmd.map Snackbar)

        Mdl msg_ ->
            Material.update Mdl msg_ model
