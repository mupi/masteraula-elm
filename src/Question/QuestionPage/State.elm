module Question.QuestionPage.State exposing (init, update)

import Http
import Navigation
import Material
import Material.Snackbar as Snackbar
import Material.Helpers exposing (map1st, map2nd)


-- My Modules

import App.Types as App
import Question.QuestionPage.Rest exposing (..)
import Question.QuestionPage.Types exposing (..)
import Question.QuestionList.State as QuestionList
import Utils.StringUtils as StringUtils


initQuestionPage : QuestionPage
initQuestionPage =
    QuestionPage 0 0 Nothing Nothing []


initFilters : Filter
initFilters =
    Filter [] False [] False [] False [] ""


init : Model
init =
    Model
        initQuestionPage
        QuestionList.init
        initFilters
        []
        False
        ""
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
        GetQuestionPage questionPage ->
            if model.questionPage.actual == questionPage && emptyFilters model.filters then
                model ! []
            else
                { model | loading = True } ! [ fetchGetQuestionPage questionPage global.token, fetchGetSubject global.token ]

        GetQuestionPageSearch questionPage ->
            if model.questionPage.actual == questionPage && not (emptyFilters model.filters) then
                model ! []
            else
                { model | loading = True } ! [ fetchGetQuestionFilterSearch questionPage model.filters global.token, fetchGetSubject global.token ]

        ChangePage page ->
            if emptyFilters model.filters then
                { model | loading = True } ! [ fetchGetQuestionPage page global.token ]
            else
                { model | loading = True } ! [ fetchGetQuestionFilterSearch page model.filters global.token ]

        QuestionListMsg subMsg ->
            let
                ( updatedQuestionList, cmd ) =
                    QuestionList.update subMsg model.questionList global
            in
                { model | questionList = updatedQuestionList } ! []

        SelectedQuestions ->
            model ! [ Navigation.newUrl "#questions/questionlist/" ]

        UpdateQuestionList questionList ->
            { model | questionList = questionList } ! []

        TagSearchInput newCurrentTag ->
            let
                filters =
                    model.filters

                newFilters =
                    { filters | currentTag = newCurrentTag }
            in
                { model | filters = newFilters } ! []

        TagSearchAdd ->
            let
                newFilters =
                    let
                        filters =
                            model.filters

                        newtags =
                            if (StringUtils.removeSpaces model.filters.currentTag) == "" then
                                model.filters.tags
                            else
                                model.filters.currentTag :: model.filters.tags
                    in
                        { filters | tags = newtags, currentTag = "" }
            in
                { model | filters = newFilters, loading = True } ! [ fetchGetQuestionFilterSearch 1 newFilters global.token ]

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

        TagSearch ->
            let
                newFilters =
                    let
                        filters =
                            model.filters

                        newTags =
                            if (StringUtils.removeSpaces model.filters.currentTag) == "" then
                                model.filters.tags
                            else
                                model.filters.currentTag :: model.filters.tags
                    in
                        { filters | tags = newTags, currentTag = "" }
            in
                { model | filters = newFilters, loading = True } ! [ fetchGetQuestionFilterSearch 1 newFilters global.token ]

        QuestionClick question ->
            model ! [ Navigation.newUrl <| String.concat [ "#question/", toString question.id ] ]

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

        OnFetchGetQuestionPage (Ok questionPage) ->
            { model | questionPage = questionPage, loading = False } ! [ Navigation.newUrl <| String.concat [ "#questions/", toString questionPage.actual ] ]

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

        OnFetchGetSubjects (Ok subjectList) ->
            { model | subjects = subjectList, error = "", loading = False } ! []

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
                { model | error = errorMsg, loading = False } ! []

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
