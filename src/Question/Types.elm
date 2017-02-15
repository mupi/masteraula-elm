module Question.Types exposing (..)

import Http
import Material


type alias Model =
    { question : Question
    , questionPage : QuestionPage
    , search : String
    , error : String
    , mdl : Material.Model
    }


type alias Question =
    { id : Int
    , question_header : String
    , question_text : String
    , level : Maybe String
    , author : String
    , credit_cost : Int
    , url : String
    , tags : List String
    , answers : List Answer
    }


type alias QuestionPage =
    { count : Int
    , actual : Int
    , next : Maybe String
    , previous : Maybe String
    , questions : List Question
    }


type alias QuestionId =
    Int


type alias PageNumber =
    Int


type alias Answer =
    { id : Int
    , answer_text : String
    , is_corect : Bool
    }


type Msg
    = GetQuestion QuestionId
    | GetQuestionPage PageNumber
    | GetQuestionTagSearch PageNumber
    | ChangePage PageNumber
    | TagSearchInput String
    | TagSearch
    | OnFetchGetQuestion (Result Http.Error Question)
    | OnFetchGetQuestionPage (Result Http.Error QuestionPage)
    | OnFetchGetQuestionTagSearch (Result Http.Error QuestionPage)
    | NoOp
    | Mdl (Material.Msg Msg)
