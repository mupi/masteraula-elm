module Question.State exposing (init, update)

import App.Config as Config
import Http
import Navigation
import Material
import Material.Snackbar as Snackbar
import Material.Helpers exposing (map1st, map2nd)


-- My Modules

import App.Types as App
import Question.Types exposing (..)
import Question.Rest exposing (..)
import Question.Question.State as Question
import Question.QuestionList.State as QuestionList
import Question.QuestionList.Rest as QuestionList
import Utils.StringUtils as StringUtils


initQuestionPage : QuestionPage
initQuestionPage =
    QuestionPage 0 0 Nothing Nothing []


initQuestionListPage : QuestionListPage
initQuestionListPage =
    QuestionListPage 0 0 Nothing Nothing []


initFilters : Filter
initFilters =
    Filter [] False [] False [] False []


init : Model
init =
    Model
        Question.init
        QuestionList.init
        Question.initQuestion
        initQuestionPage
        QuestionList.initQuestionList
        QuestionList.initQuestionList
        []
        ""
        initFilters
        []
        False
        False
        False
        False
        False
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
        QuestionMsg subMsg ->
            let
                ( updatedQuestion, cmd ) =
                    Question.update subMsg model.questionModel global
            in
                { model | questionModel = updatedQuestion } ! []

        QuestionListMsg subMsg ->
            let
                ( updatedQuestionList, cmd ) =
                    QuestionList.update subMsg model.questionListModel global
            in
                { model | questionListModel = updatedQuestionList } ! []

        -- GetQuestion questionId ->
        --     if model.question.id == questionId then
        --         { model | selectingQuestions = False } ! []
        --     else
        --         { model | selectingQuestions = False, redirected = True, loading = True } ! [ Question.fetchGetQuestion questionId global.token ]
        GetQuestionPage questionPage ->
            if model.questionPage.actual == questionPage && emptyFilters model.filters then
                { model | selectingQuestions = True } ! []
            else
                { model | filters = initFilters, selectingQuestions = True, redirected = True, loading = True } ! [ fetchGetQuestionPage questionPage global.token, fetchGetSubject global.token ]

        GetQuestionPageSearch questionPage ->
            if model.questionPage.actual == questionPage && not (emptyFilters model.filters) then
                { model | selectingQuestions = True } ! []
            else
                { model | selectingQuestions = True, redirected = True, loading = True } ! [ fetchGetQuestionFilterSearch questionPage model.filters global.token, fetchGetSubject global.token ]

        GetMineQuestionListPage questionPage ->
            { model | selectingQuestions = False } ! [ fetchGetMineQuestionList questionPage global.token ]

        -- GetQuestionList questionListId ->
        --     let
        --         newFilters =
        --             let
        --                 filters =
        --                     model.filters
        --             in
        --                 { filters | tags = [] }
        --     in
        --         if model.questionListSelected.id == questionListId then
        --             model ! []
        --         else
        --             { model | filters = newFilters, selectingQuestions = False, redirected = True, loading = True } ! [ Cmd.map GetQuestionListTest <| QuestionList.fetchGetQuestionList questionListId global.token ]
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
                { model | loading = True } ! [ fetchGetQuestionPage page global.token ]
            else
                { model | loading = True } ! [ fetchGetQuestionFilterSearch page model.filters global.token ]

        TagSearchInput newCurrentTag ->
            { model | currentTag = newCurrentTag } ! []

        TagSearch ->
            let
                newFilters =
                    let
                        filters =
                            model.filters

                        newTags =
                            if (StringUtils.removeSpaces model.currentTag) == "" then
                                model.filters.tags
                            else
                                model.currentTag :: model.filters.tags
                    in
                        { filters | tags = newTags }
            in
                { model | filters = newFilters, currentTag = "", loading = True } ! [ fetchGetQuestionFilterSearch 1 newFilters global.token ]

        TagSearchAdd ->
            let
                newFilters =
                    let
                        filters =
                            model.filters

                        newtags =
                            if (StringUtils.removeSpaces model.currentTag) == "" then
                                model.filters.tags
                            else
                                model.currentTag :: model.filters.tags
                    in
                        { filters | tags = newtags }
            in
                { model | filters = newFilters, currentTag = "", loading = True } ! [ fetchGetQuestionFilterSearch 1 newFilters global.token ]

        TagSearchRemove tag ->
            let
                newFilters =
                    let
                        filters =
                            model.filters

                        newTags =
                            List.filterMap
                                (\t ->
                                    if t == tag then
                                        Nothing
                                    else
                                        Just t
                                )
                                model.filters.tags
                    in
                        { filters | tags = newTags }
            in
                if emptyFilters newFilters then
                    { model | filters = newFilters, loading = True } ! [ fetchGetQuestionPage 1 global.token ]
                else
                    { model | filters = newFilters, loading = True } ! [ fetchGetQuestionFilterSearch 1 newFilters global.token ]

        QuestionClick question ->
            { model | question = question } ! [ Navigation.newUrl <| String.concat [ "#question/", toString question.id ] ]

        QuestionBack ->
            model ! [ Navigation.back 1 ]

        -- QuestionListAdd question ->
        --     let
        --         questionList =
        --             model.questionListEdit
        --
        --         updQuestionList =
        --             let
        --                 insert : Question.Question -> List QuestionList.QuestionOrder -> List QuestionList.QuestionOrder
        --                 insert question list =
        --                     case list of
        --                         qo :: rest ->
        --                             if Question.questionParent question /= Nothing && question.question_parent == qo.question.question_parent then
        --                                 QuestionList.QuestionOrder question 0 :: qo :: rest
        --                             else
        --                                 qo :: insert question rest
        --
        --                         [] ->
        --                             [ QuestionList.QuestionOrder question 0 ]
        --             in
        --                 { questionList | questions = List.indexedMap (\index questionOrder -> { questionOrder | order = index + 1 }) <| insert question questionList.questions }
        --     in
        --         { model | questionListEdit = updQuestionList } ! []
        --
        -- QuestionListRemove question ->
        --     let
        --         questionList =
        --             model.questionListEdit
        --
        --         newQuestions =
        --             List.filterMap
        --                 (\questionOrder ->
        --                     if questionOrder.question == question then
        --                         Nothing
        --                     else
        --                         Just questionOrder
        --                 )
        --                 questionList.questions
        --
        --         newQuestionList =
        --             { questionList
        --                 | questions = List.indexedMap (\index questionOrder -> { questionOrder | order = index }) newQuestions
        --             }
        --     in
        --         { model | questionListEdit = newQuestionList } ! []
        -- QuestionListHeaderInput newQuestionHeader ->
        --     let
        --         questionList =
        --             model.questionListEdit
        --
        --         newQuestionList =
        --             { questionList | question_list_header = newQuestionHeader }
        --     in
        --         { model | questionListEdit = newQuestionList } ! []
        -- QuestionListSave ->
        --     let
        --         ( valid, error ) =
        --             let
        --                 question_list_header =
        --                     model.questionListEdit.question_list_header
        --
        --                 question_list =
        --                     model.questionListEdit
        --             in
        --                 if question_list_header == "" then
        --                     ( False, "Por favor, coloque o nome da lista antes de salvá-la" )
        --                 else if List.length question_list.questions <= 0 then
        --                     ( False, "Por favor, adicione pelo menos uma questão antes de salvá-la" )
        --                 else if (not <| StringUtils.hasNoSpecialCharacters question_list_header) then
        --                     ( False, "Por favor, coloque apenas letras (sem acentos), números e espaços no nome da lista." )
        --                 else
        --                     ( True, "" )
        --     in
        --         if not valid then
        --             let
        --                 ( snackbar, effect ) =
        --                     let
        --                         contents =
        --                             Snackbar.snackbar 0 error "Fechar"
        --                     in
        --                         Snackbar.add ({ contents | timeout = 5000 }) model.snackbar
        --                             |> map2nd (Cmd.map Snackbar)
        --             in
        --                 { model | snackbar = snackbar } ! [ effect ]
        --         else
        --             { model | generateAfterSave = False, loading = True } ! [ QuestionList.fetchPostSaveQuestionList model.questionListEdit global.token ]
        QuestionListClear ->
            let
                questionListEdit =
                    model.questionListEdit

                newQuestionList =
                    { questionListEdit | questions = [] }
            in
                { model | questionListEdit = newQuestionList } ! []

        QuestionListGenerate questionList ->
            { model | downloading = True, generateWithAnswer = False, generateWithResolution = False } ! [ fetchGetGenerateList questionList.id global.token model ]

        -- QuestionListDelete ->
        --     model ! [ QuestionList.fetchDeleteQuestionList model.questionListEdit global.token ]
        QuestionListClick questionListId ->
            { model | loading = True } ! [ Cmd.map QuestionListMsg <| QuestionList.fetchGetQuestionList questionListId global.token ]

        QuestionListEdit questionList ->
            { model | questionListEdit = questionList } ! [ Navigation.newUrl <| String.concat [ "#questions/questionlist" ] ]

        ToggleGenerateWithAnswer ->
            { model | generateWithAnswer = not model.generateWithAnswer } ! []

        ToggleGenerateWithResolution ->
            { model | generateWithResolution = not model.generateWithResolution } ! []

        QuestionListCancel ->
            let
                questionListId =
                    model.questionListEdit.id
            in
                { model | questionListEdit = QuestionList.initQuestionList } ! [ Navigation.newUrl <| String.concat [ "#questions/questionlists/", toString questionListId ] ]

        FilterLevel newFilter ->
            let
                newFilters =
                    let
                        filters =
                            model.filters

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
                    in
                        { filters | levelFilters = newlevelFilters }
            in
                if emptyFilters newFilters then
                    { model | filters = newFilters, loading = True } ! [ fetchGetQuestionPage 1 global.token ]
                else
                    { model | filters = newFilters, loading = True } ! [ fetchGetQuestionFilterSearch 1 newFilters global.token ]

        FilterSubject newFilter ->
            let
                newFilters =
                    let
                        filters =
                            model.filters

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
                    in
                        { filters | subjectFilters = newsubjectFilters }
            in
                if emptyFilters newFilters then
                    { model | filters = newFilters, loading = True } ! [ fetchGetQuestionPage 1 global.token, fetchGetSubject global.token ]
                else
                    { model | filters = newFilters, loading = True } ! [ fetchGetQuestionFilterSearch 1 newFilters global.token ]

        FilterEducationLevel newFilter ->
            let
                newFilters =
                    let
                        filters =
                            model.filters

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
                    in
                        { filters | educationLevelFilters = neweducationLevelFilter }
            in
                if emptyFilters newFilters then
                    { model | filters = newFilters, loading = True } ! [ fetchGetQuestionPage 1 global.token, fetchGetSubject global.token ]
                else
                    { model | filters = newFilters, loading = True } ! [ fetchGetQuestionFilterSearch 1 newFilters global.token ]

        ToggleFilter toogle ->
            let
                filters =
                    model.filters

                newFilters =
                    case toogle of
                        LevelToggle ->
                            { filters | levelToggle = not filters.levelToggle }

                        SubjectToggle ->
                            { filters | subjectToggle = not filters.subjectToggle }

                        EducationToggle ->
                            { filters | educationToggle = not filters.educationToggle }
            in
                { model | filters = newFilters } ! []

        -- OnFetchGetQuestion (Ok question) ->
        --     if model.redirected then
        --         { model | question = question, error = "", redirected = False, loading = False } ! []
        --     else
        --         { model | question = question, error = "", loading = False } ! [ Navigation.newUrl <| String.concat [ "#question/", toString question.id ] ]
        --
        -- OnFetchGetQuestion (Err error) ->
        --     let
        --         errorMsg =
        --             case error of
        --                 Http.NetworkError ->
        --                     "Erro de conexão com o servidor"
        --
        --                 Http.BadStatus response ->
        --                     "Questão inexistente"
        --
        --                 _ ->
        --                     toString error
        --     in
        --         { model | error = errorMsg } ! []
        OnFetchGetQuestionPage (Ok questionPage) ->
            if model.redirected then
                { model | questionPage = questionPage, error = "", redirected = False, loading = False } ! []
            else
                { model | questionPage = questionPage, error = "", loading = False } ! [ Navigation.newUrl <| String.concat [ "#questions/", toString questionPage.actual ] ]

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
                { model | error = errorMsg, loading = False } ! []

        OnFetchGetQuestionFilterSearch (Ok questionPage) ->
            if model.redirected then
                { model | questionPage = questionPage, error = "", loading = False, redirected = False } ! []
            else
                { model | questionPage = questionPage, error = "", loading = False } ! [ Navigation.newUrl <| String.concat [ "#questions/tagsearch/", toString questionPage.actual ] ]

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
                { model | error = errorMsg, loading = False } ! []

        OnFecthQuestionListGenerate (Ok id) ->
            let
                newUrl =
                    String.concat [ Config.baseUrl, "question_lists/", id, "/get_list/" ]
            in
                { model | error = "", downloading = False } ! [ Navigation.load newUrl ]

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
                { model | error = errorMsg, downloading = False } ! []

        -- OnFetchSaveQuestionList (Ok newId) ->
        --     let
        --         newQuestionList =
        --             let
        --                 questionList =
        --                     model.questionListEdit
        --             in
        --                 { questionList | id = newId }
        --
        --         cmds =
        --             if model.generateAfterSave then
        --                 [ fetchGetGenerateList newId global.token model ]
        --             else
        --                 [ Navigation.newUrl <| String.concat [ "#questions/questionlists/", toString newId ] ]
        --     in
        --         { model | questionListSelected = newQuestionList, questionListEdit = QuestionList.initQuestionList, generateAfterSave = False, loading = False } ! cmds
        --
        -- OnFetchSaveQuestionList (Err error) ->
        --     let
        --         errorMsg =
        --             case error of
        --                 Http.NetworkError ->
        --                     "Erro de conexão com o servidor"
        --
        --                 Http.BadStatus response ->
        --                     "Página inexistente"
        --
        --                 _ ->
        --                     toString error
        --     in
        --         { model | error = errorMsg, loading = False } ! []
        -- OnFetchDeleteQuestionList (Ok text) ->
        --     { model | questionListEdit = QuestionList.initQuestionList } ! [ Navigation.newUrl "#questions/user_lists/1" ]
        --
        -- OnFetchDeleteQuestionList (Err error) ->
        --     let
        --         errorMsg =
        --             case error of
        --                 Http.NetworkError ->
        --                     "Erro de conexão com o servidor"
        --
        --                 Http.BadStatus response ->
        --                     "Página inexistente"
        --
        --                 _ ->
        --                     toString error
        --
        --         ( snackbar, effect ) =
        --             Snackbar.add (Snackbar.snackbar 0 errorMsg "Close") model.snackbar
        --                 |> map2nd (Cmd.map Snackbar)
        --     in
        --         { model | error = errorMsg, snackbar = snackbar } ! [ effect ]
        OnFetchGetMineQuestionListPage (Ok mineQuestionLists) ->
            { model | mineQuestionLists = mineQuestionLists, error = "" } ! []

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

        -- OnFetchGetQuestionList (Ok questionList) ->
        --     if model.redirected then
        --         { model | questionListSelected = questionList, error = "", loading = False, redirected = False } ! []
        --     else
        --         { model | questionListSelected = questionList, error = "", loading = False } ! [ Navigation.newUrl <| String.concat [ "#questions/questionlists/", toString questionList.id ] ]
        --
        -- OnFetchGetQuestionList (Err error) ->
        --     let
        --         errorMsg =
        --             case error of
        --                 Http.NetworkError ->
        --                     "Erro de conexão com o servidor"
        --
        --                 Http.BadStatus response ->
        --                     "Página inexistente"
        --
        --                 _ ->
        --                     toString error
        --     in
        --         { model | error = errorMsg } ! []
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
