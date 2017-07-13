module Question.SelectedQuestionList.Types exposing (..)

import Material
import Material.Snackbar as Snackbar
import Question.Question.Types as Question
import Question.QuestionList.Types as QuestionList
import Question.QuestionListGenerate.Types as QuestionListGenerate


type alias Model =
    { questionList : QuestionList.Model
    , questionListGenerate : QuestionListGenerate.Model
    , -- MDL
      loading : Bool
    , snackbar : Snackbar.Model Int
    , mdl : Material.Model
    }


type Msg
    = GetQuestionList Int
      -- Controls
    | QuestionClick Question.Question
    | QuestionListEdit
    | UpdateQuestionList QuestionList.Model
      -- Msg
    | QuestionListGenerateMsg QuestionListGenerate.Msg
    | QuestionListMsg QuestionList.Msg
      -- General
    | Dialog
    | NoOp
    | Snackbar (Snackbar.Msg Int)
    | Mdl (Material.Msg Msg)
