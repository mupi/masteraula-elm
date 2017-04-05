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
    Question 0 "" Nothing User.initUser 0 [] Nothing [] [] Nothing Nothing Nothing


initQuestionPage : QuestionPage
initQuestionPage =
    QuestionPage 0 1 Nothing Nothing []


initQuestionListPage : QuestionListPage
initQuestionListPage =
    QuestionListPage 0 1 Nothing Nothing []


initQuestionList : QuestionList
initQuestionList =
    QuestionList 0 "" False User.initUser [] ""


initFilters : Filter
initFilters =
    Filter [] [] [] []


init : Model
init =
    Model
        initQuestion
        initQuestionPage
        initQuestionList
        initQuestionList
        initQuestionListPage
        ""
        initFilters
        []
        False
        False
        ""
        Delete
        Snackbar.model
        Material.model


emptyFilters : Filter -> Bool
emptyFilters filters =
    filters.tags
        == []
        && filters.levelFilters
        == []
        && filters.subjectFilters
        == []
        && filters.educationLevelFilters
        == []


update : Msg -> Model -> App.Global -> ( Model, Cmd Msg )
update msg model global =
    case msg of
        GetQuestion questionId ->
            { model | selectingQuestions = False } ! [ fetchGetQuestion questionId global.token ]

        GetQuestionPage questionPage ->
            let
                newFilters =
                    let
                        filters =
                            model.filters
                    in
                        { filters | tags = [], levelFilters = [], subjectFilters = [] }
            in
                { model | filters = newFilters, selectingQuestions = True } ! [ fetchGetQuestionPage questionPage global.token, fetchGetSubject global.token ]

        GetMineQuestionListPage questionPage ->
            let
                newFilters =
                    let
                        filters =
                            model.filters
                    in
                        { filters | tags = [] }
            in
                { model | filters = newFilters, selectingQuestions = False } ! [ fetchGetMineQuestionList questionPage global.token ]

        GetQuestionList questionListId ->
            let
                newFilters =
                    let
                        filters =
                            model.filters
                    in
                        { filters | tags = [] }
            in
                { model | filters = newFilters, selectingQuestions = False } ! [ fetchGetQuestionList questionListId global.token ]

        GetQuestionTagSearch questionPage ->
            { model | selectingQuestions = True } ! [ fetchGetQuestionFilterSearch questionPage model.filters global.token ]

        DrawerLinkClick drawerLink ->
            case drawerLink of
                MineLists ->
                    { model | selectingQuestions = False } ! [ Navigation.newUrl "#questions/user_lists/1" ]

                SelectQuestions ->
                    { model | selectingQuestions = True } ! [ Navigation.newUrl "#questions/1" ]

                SelectedQuestions ->
                    { model | selectingQuestions = False } ! [ Navigation.newUrl "#questions/questionlist/" ]

        ChangePage page ->
            if emptyFilters model.filters then
                model ! [ Navigation.newUrl <| String.concat [ "#questions/", toString page ] ]
            else
                model ! [ Navigation.newUrl <| String.concat [ "#questions/tagsearch/", toString page ] ]

        TagSearchInput newCurrentTag ->
            { model | currentTag = newCurrentTag } ! []

        TagSearch ->
            let
                newTags =
                    if (StringUtils.removeSpaces model.currentTag) == "" then
                        model.filters.tags
                    else
                        model.currentTag :: model.filters.tags

                newFilters =
                    let
                        filters =
                            model.filters
                    in
                        { filters | tags = newTags }
            in
                { model | filters = newFilters, currentTag = "" } ! [ Navigation.newUrl "#questions/tagsearch/1" ]

        TagSearchAdd ->
            let
                tags =
                    if (StringUtils.removeSpaces model.currentTag) == "" then
                        model.filters.tags
                    else
                        model.currentTag :: model.filters.tags

                newFilters =
                    let
                        filters =
                            model.filters
                    in
                        { filters | tags = tags }
            in
                { model | filters = newFilters, currentTag = "" } ! [ Navigation.newUrl "#questions/tagsearch/1" ]

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
                        model.filters.tags

                newFilters =
                    let
                        filters =
                            model.filters
                    in
                        { filters | tags = newTags }
            in
                if emptyFilters newFilters then
                    { model | filters = newFilters } ! [ Navigation.newUrl <| "#questions/1" ]
                else
                    { model | filters = newFilters } ! [ Navigation.newUrl <| "#questions/tagsearch/1" ]

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

        FilterLevel newFilter ->
            let
                newlevelFilters =
                    let
                        levelFilters =
                            model.filters.levelFilters
                    in
                        case newFilter of
                            AllLevel ->
                                []

                            _ ->
                                if List.member newFilter levelFilters then
                                    List.filter (\level -> level /= newFilter) levelFilters
                                else
                                    newFilter :: levelFilters

                newFilters =
                    let
                        filters =
                            model.filters
                    in
                        { filters | levelFilters = newlevelFilters }
            in
                if emptyFilters newFilters then
                    { model | filters = newFilters } ! [ Navigation.newUrl <| "#questions/1" ]
                else
                    { model | filters = newFilters } ! [ Navigation.newUrl <| "#questions/tagsearch/1" ]

        FilterSubject newFilter ->
            let
                newsubjectFilters =
                    let
                        subjectFilters =
                            model.filters.subjectFilters
                    in
                        case newFilter of
                            AllSubject ->
                                []

                            StringSubject subjectFilter ->
                                if List.member subjectFilter subjectFilters then
                                    List.filter (\subject -> subject /= subjectFilter) subjectFilters
                                else
                                    subjectFilter :: subjectFilters

                newFilters =
                    let
                        filters =
                            model.filters
                    in
                        { filters | subjectFilters = newsubjectFilters }
            in
                if emptyFilters newFilters then
                    { model | filters = newFilters } ! [ Navigation.newUrl <| "#questions/1" ]
                else
                    { model | filters = newFilters } ! [ Navigation.newUrl <| "#questions/tagsearch/1" ]

        FilterEducationLevel newFilter ->
            let
                neweducationLevelFilter =
                    let
                        educationLevelFilters =
                            model.filters.educationLevelFilters
                    in
                        case newFilter of
                            AllEducationLevel ->
                                []

                            StringEducationLevel educationLevelFilter ->
                                if List.member educationLevelFilter educationLevelFilters then
                                    List.filter (\level -> level /= educationLevelFilter) educationLevelFilters
                                else
                                    educationLevelFilter :: educationLevelFilters

                newFilters =
                    let
                        filters =
                            model.filters
                    in
                        { filters | educationLevelFilters = neweducationLevelFilter }
            in
                if emptyFilters newFilters then
                    { model | filters = newFilters } ! [ Navigation.newUrl <| "#questions/1" ]
                else
                    { model | filters = newFilters } ! [ Navigation.newUrl <| "#questions/tagsearch/1" ]

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

        OnFetchGetSubjects (Ok subjectList) ->
            { model | subjects = subjectList, error = "" } ! []

        OnFetchGetSubjects (Err error) ->
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
