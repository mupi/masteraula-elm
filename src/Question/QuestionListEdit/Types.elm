module Question.QuestionListEdit.Types exposing (..)

import Material
import Material.Snackbar as Snackbar
import Question.Question.Types as Question
import Question.QuestionList.Types as QuestionList
import Question.QuestionListGenerate.Types as QuestionListGenerate


type alias Model =
    { questionList : QuestionList.Model
    , questionListGenerate : QuestionListGenerate.Model
    , error : String
    , -- MDL
      dialog : DialogType
    , snackbar : Snackbar.Model Int
    , mdl : Material.Model
    }


type DialogType
    = Delete
    | Clear


type Msg
    = QuestionListEdit QuestionList.QuestionList
    | QuestionListHeaderInput String
      -- Controls
    | QuestionListCancel
    | QuestionClick Question.Question
    | SelectQuestions
      -- UpdateQuestionList
    | UpdateQuestionList QuestionList.Model
      -- Fetch
    | QuestionListMsg QuestionList.Msg
      -- General
    | Dialog DialogType
    | NoOp
    | Snackbar (Snackbar.Msg Int)
    | Mdl (Material.Msg Msg)
