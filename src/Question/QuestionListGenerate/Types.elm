module Question.QuestionListGenerate.Types exposing (..)

import Http
import Material
import Question.QuestionList.Types as QuestionList


type alias Model =
    { generateAfterSave : Bool
    , generateWithAnswer : Bool
    , generateWithResolution : Bool
    , downloading : Bool
    , error : String
    , -- MDL
      mdl : Material.Model
    }


type Msg
    = QuestionListGenerate QuestionList.QuestionList
      -- Question List Page
    | ToggleGenerateWithAnswer
    | ToggleGenerateWithResolution
      -- Filter
    | OnFecthQuestionListGenerate (Result Http.Error String)
    | NoOp
    | Mdl (Material.Msg Msg)
