module Question.QuestionList.State exposing (init, update, initQuestionList)

import Http
import Navigation
import Material


-- My modules

import App.Types as App
import User.State as User
import Question.QuestionList.Types exposing (..)
import Question.QuestionList.Rest exposing (..)
import Question.Question.State as Question
import Question.Question.Types as Question


initQuestionList : QuestionList
initQuestionList =
    QuestionList
        0
        ""
        False
        User.initUser
        []
        0
        ""


init : Model
init =
    Model
        initQuestionList
        ""
        Material.model


update : Msg -> Model -> App.Global -> ( Model, Cmd Msg )
update msg model global =
    case msg of
        GetQuestionList questionListId ->
            model ! [ fetchGetQuestionList questionListId global.token ]

        -- let
        --     newFilters =
        --         let
        --             filters =
        --                 model.filters
        --         in
        --             { filters | tags = [] }
        -- in
        --     if model.questionListSelected.id == questionListId then
        --         model ! []
        --     else
        --         { model | filters = newFilters, selectingQuestions = False, redirected = True, loading = True } ! [ fetchGetQuestionList questionListId global.token ]
        QuestionListAdd question ->
            let
                questionList =
                    model.questionList

                updQuestionList =
                    let
                        insert : Question.Question -> List QuestionOrder -> List QuestionOrder
                        insert question list =
                            case list of
                                qo :: rest ->
                                    if Question.questionParent question /= Nothing && question.question_parent == qo.question.question_parent then
                                        QuestionOrder question 0 :: qo :: rest
                                    else
                                        qo :: insert question rest

                                [] ->
                                    [ QuestionOrder question 0 ]
                    in
                        { questionList | questions = List.indexedMap (\index questionOrder -> { questionOrder | order = index + 1 }) <| insert question questionList.questions }
            in
                { model | questionList = updQuestionList } ! []

        QuestionListRemove question ->
            let
                questionList =
                    model.questionList

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
                { model | questionList = newQuestionList } ! []

        OnFetchGetQuestionList (Ok questionList) ->
            -- if model.redirected then
            --     { model | questionList = questionList, error = "", loading = False, redirected = False } ! []
            -- else
            --     { model | questionList = questionList, error = "", loading = False } ! [ Navigation.newUrl <| String.concat [ "#questions/questionlists/", toString questionList.id ] ]
            { model | questionList = questionList, error = "" } ! [ Navigation.newUrl <| String.concat [ "#questions/questionlists/", toString questionList.id ] ]

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

        Mdl msg_ ->
            Material.update Mdl msg_ model
