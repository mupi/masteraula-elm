module Question.Question.Types exposing (..)

import Http
import Material
import User.Types as User


type alias Model =
    { question : Question
    , error : String
    , mdl : Material.Model
    }


type alias Question =
    { id : Int
    , question_parent : QuestionParent
    , question_statement : String
    , level : Maybe String
    , author : User.User
    , credit_cost : Int
    , tags : List String
    , resolution : Maybe String
    , answers : List Answer
    , subjects : List Subject
    , education_level : Maybe String
    , year : Maybe Int
    , source : Maybe String
    , question_lists : List QuestionListInfo
    , related_questions : RelatedQuestion
    }


type QuestionParent
    = QuestionParent (Maybe Question)


type RelatedQuestion
    = RelatedQuestion (List Question)


type alias Answer =
    { id : Int
    , answer_text : String
    , is_correct : Bool
    }


type alias Subject =
    { id : Int
    , subject_name : String
    , slug : String
    }


type alias QuestionListInfo =
    { id : Int
    , question_list_header : String
    , secret : Bool
    , owner : User.User
    , question_count : Int
    , create_date : String
    }


type alias QuestionId =
    Int


type Msg
    = GetQuestion QuestionId
      -- Fetch
    | OnFetchGetQuestion (Result Http.Error Question)
    | Mdl (Material.Msg Msg)
