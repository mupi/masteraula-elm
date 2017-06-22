module Question.QuestionList.Types exposing (..)

import Http
import Material
import User.Types as User
import Question.Question.Types as Question


type alias Model =
    { questionList : QuestionList
    , error : String
    , mdl : Material.Model
    }


type alias QuestionList =
    { id : Int
    , question_list_header : String
    , secret : Bool
    , owner : User.User
    , questions : List QuestionOrder
    , question_count : Int
    , create_date : String
    }


type alias QuestionOrder =
    { question : Question.Question
    , order : Int
    }


type alias QuestionListId =
    Int


type Msg
    = GetQuestionList QuestionListId
      -- QuestionList (on Edit)
    | QuestionListAdd Question.Question
    | QuestionListRemove Question.Question
      -- Fetch
    | OnFetchGetQuestionList (Result Http.Error QuestionList)
    | Mdl (Material.Msg Msg)
