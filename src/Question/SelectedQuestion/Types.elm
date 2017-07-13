module Question.SelectedQuestion.Types exposing (..)

import Material
import Question.Question.Types as Question
import Question.QuestionList.Types as QuestionList


type alias Model =
    { question : Question.Model
    , questionList : QuestionList.Model
    , loading : Bool
    , error : String
    , -- MDL
      mdl : Material.Model
    }


type QuestionButtonType
    = AddQuestionButton
    | RemoveQuestionButton
    | NoneQuestionButton


type Msg
    = GetQuestion Int
      -- Controls
    | QuestionClick Question.Question
    | QuestionBack
      -- Update Question List
    | UpdateQuestionList QuestionList.Model
      -- SubMsg
    | QuestionListMsg QuestionList.Msg
    | QuestionMsg Question.Msg
      -- General
    | NoOp
    | Mdl (Material.Msg Msg)
