module Question.QuestionListEdit.State exposing (init, update)

import App.Config as Config
import Http
import Navigation
import Material
import Material.Snackbar as Snackbar
import Material.Helpers exposing (map1st, map2nd)


-- My Modules

import App.Types as App
import Question.QuestionListEdit.Types exposing (..)
import Question.Question.Types as Question
import Question.Question.State as Question
import Question.QuestionList.Types as QuestionList
import Question.QuestionList.State as QuestionList
import Question.QuestionList.Rest as QuestionList
import Question.QuestionListGenerate.State as QuestionListGenerate
import Question.QuestionListGenerate.Types as QuestionListGenerate
import Utils.StringUtils as StringUtils


init : Model
init =
    Model
        QuestionList.init
        QuestionListGenerate.init
        ""
        Delete
        Snackbar.model
        Material.model


update : Msg -> Model -> App.Global -> ( Model, Cmd Msg )
update msg model global =
    case msg of
        QuestionListEdit questionList ->
            let
                questionListModel =
                    model.questionList

                newQuestionListModel =
                    { questionListModel | questionList = questionList }
            in
                { model | questionList = newQuestionListModel } ! []

        QuestionListHeaderInput newQuestionHeader ->
            let
                ( updatedQuestionList, cmd ) =
                    QuestionList.update (QuestionList.QuestionListHeaderInput newQuestionHeader) model.questionList global
            in
                { model | questionList = updatedQuestionList } ! []

        QuestionListGenerate questionList ->
            let
                ( updatedGenerateList, cmd ) =
                    QuestionListGenerate.update (QuestionListGenerate.QuestionListGenerate questionList) model.questionListGenerate global
            in
                -- { model | downloading = True, generateWithAnswer = False, generateWithResolution = False } ! [ fetchGetGenerateList questionList.id global.token model ]
                { model | questionListGenerate = updatedGenerateList } ! []

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
        --
        -- QuestionListDelete ->
        --     model ! [ QuestionList.fetchDeleteQuestionList model.questionListEdit global.token ]
        QuestionListCancel ->
            let
                questionListId =
                    model.questionList.questionList.id
            in
                { model | questionList = QuestionList.init } ! [ Navigation.newUrl <| String.concat [ "#questions/questionlists/", toString questionListId ] ]

        SelectQuestions ->
            model ! [ Navigation.newUrl "#questions/1" ]

        QuestionListGenerateMsg questionListMsg ->
            let
                ( updatedQuestionListGenerate, cmd ) =
                    QuestionListGenerate.update questionListMsg model.questionListGenerate global
            in
                model ! [ (Cmd.map QuestionListGenerateMsg cmd) ]

        QuestionListMsg questionListMsg ->
            let
                ( updatedQuestionList, cmd ) =
                    QuestionList.update questionListMsg model.questionList global
            in
                model ! [ (Cmd.map QuestionListMsg cmd) ]

        Dialog dialog ->
            { model | dialog = dialog } ! []

        Snackbar (Snackbar.End a) ->
            { model | snackbar = Snackbar.model } ! []

        Snackbar msg ->
            Snackbar.update msg model.snackbar
                |> map1st (\s -> { model | snackbar = s })
                |> map2nd (Cmd.map Snackbar)

        NoOp ->
            model ! []

        Mdl msg_ ->
            Material.update Mdl msg_ model
