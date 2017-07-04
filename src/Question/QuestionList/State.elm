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
import Utils.StringUtils as StringUtils


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

        QuestionListHeaderInput newQuestionHeader ->
            let
                questionList =
                    model.questionList

                newQuestionList =
                    { questionList | question_list_header = newQuestionHeader }
            in
                { model | questionList = newQuestionList } ! []

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

        QuestionListSave ->
            let
                ( valid, error ) =
                    let
                        questionList =
                            model.questionList
                    in
                        if questionList.question_list_header == "" then
                            ( False, "Por favor, coloque o nome da lista antes de salvá-la" )
                        else if List.length questionList.questions <= 0 then
                            ( False, "Por favor, adicione pelo menos uma questão antes de salvá-la" )
                        else if (not <| StringUtils.hasNoSpecialCharacters questionList.question_list_header) then
                            ( False, "Por favor, coloque apenas letras (sem acentos), números e espaços no nome da lista." )
                        else
                            ( True, "" )
            in
                if not valid then
                    { model | error = error } ! []
                else
                    { model | error = "" } ! [ fetchPostSaveQuestionList model.questionList global.token ]

        QuestionListClear ->
            let
                questionList =
                    model.questionList

                newQuestionList =
                    { questionList | questions = [] }
            in
                { model | questionList = newQuestionList } ! []

        QuestionListDelete ->
            model ! [ fetchDeleteQuestionList model.questionList global.token ]

        OnFetchSaveQuestionList (Ok newId) ->
            let
                newQuestionList =
                    let
                        questionList =
                            model.questionList
                    in
                        { questionList | id = newId }

                cmds =
                    [ Navigation.newUrl <| String.concat [ "#questions/questionlists/", toString newId ] ]
            in
                { model | questionList = newQuestionList, error = "" } ! cmds

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
            { model | questionList = initQuestionList, error = "" } ! [ Navigation.newUrl "#questions/user_lists/1" ]

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

        OnFetchGetQuestionList (Ok questionList) ->
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
