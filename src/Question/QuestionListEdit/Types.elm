module Question.QuestionListEdit.Types exposing (..)

import Material
import Material.Snackbar as Snackbar
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
    | GenerateList QuestionList.QuestionList


type QuestionButtonType
    = AddQuestionButton
    | RemoveQuestionButton
    | NoneQuestionButton


type Msg
    = QuestionListEdit QuestionList.QuestionList
      -- QuestionList (on Edit)
      -- | QuestionListAdd Question.Question
      -- | QuestionListRemove Question.Question
    | QuestionListHeaderInput String
      -- | QuestionListSave
      -- | QuestionListClear
    | QuestionListGenerate QuestionList.QuestionList
      -- | QuestionListDelete
    | QuestionListCancel
    | SelectQuestions
      -- QuestionList Generate
    | QuestionListGenerateMsg QuestionListGenerate.Msg
    | QuestionListMsg QuestionList.Msg
      -- Filter
      -- | OnFecthQuestionListGenerate (Result Http.Error String)
    | Dialog DialogType
    | NoOp
    | Snackbar (Snackbar.Msg Int)
    | Mdl (Material.Msg Msg)
