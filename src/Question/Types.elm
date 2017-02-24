module Question.Types exposing (..)

import Http
import Material
import User.Types as User


type alias Model =
    { question : Question
    , questionPage : QuestionPage
    , questionList : QuestionList
    , -- Search
      currentTag : String
    , tags : List String
    , filterId : Int
    , -- List docx file generation
      generateAfterSave : Bool
    , error : String
    , mdl : Material.Model
    }


type alias Question =
    { id : Int
    , question_header : String
    , question_text : String
    , level : Maybe String
    , author : User.Model
    , credit_cost : Int
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


type alias QuestionList =
    { id : Int
    , question_list_header : String
    , secret : Bool
    , owner : String
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
      -- Tags
    | TagSearchInput String
    | TagSearchAdd
    | TagSearchRemove String
    | TagSearch
      -- QuestionList
    | QuestionListAdd Question
    | QuestionListRemove Question
    | QuestionListHeaderInput String
    | QuestionListSave
    | QuestionListClear
    | QuestionListGenerate
    | QuestionListDelete
      -- Filter
    | Filter Int
      -- Fetch
    | OnFetchGetQuestion (Result Http.Error Question)
    | OnFetchGetQuestionPage (Result Http.Error QuestionPage)
    | OnFetchGetQuestionFilterSearch (Result Http.Error QuestionPage)
    | OnFecthQuestionListGenerate (Result Http.Error String)
    | OnFetchSaveQuestionList (Result Http.Error Int)
    | OnFetchDeleteQuestionList (Result Http.Error String)
    | NoOp
    | Mdl (Material.Msg Msg)
