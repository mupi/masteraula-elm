module Question.Types exposing (..)

import Http
import Material
import Material.Snackbar as Snackbar
import User.Types as User


type alias Model =
    { question : Question
    , questionPage : QuestionPage
    , questionListEdit : QuestionList
    , questionListSelected : QuestionList
    , questionListPage : QuestionListPage
    , -- Search
      currentTag : String
    , tags : List String
    , filterId : Int
    , -- List docx file generation
      generateAfterSave : Bool
    , error : String
    , dialog : DialogType
    , snackbar : Snackbar.Model Int
    , mdl : Material.Model
    }


type alias Question =
    { id : Int
    , question_statement : String
    , level : Maybe String
    , author : User.User
    , credit_cost : Int
    , tags : List String
    , answers : List Answer
    , subjects : List Subject
    }


type alias Subject =
    { id : Int
    , subject_name : String
    }


type alias QuestionOrder =
    { question : Question
    , order : Int
    }


type alias QuestionList =
    { id : Int
    , question_list_header : String
    , secret : Bool
    , owner : User.User
    , questions : List QuestionOrder
    , create_date : String
    }


type alias QuestionPage =
    { count : Int
    , actual : Int
    , next : Maybe String
    , previous : Maybe String
    , questions : List Question
    }


type alias QuestionListPage =
    { count : Int
    , actual : Int
    , next : Maybe String
    , previous : Maybe String
    , questionLists : List QuestionList
    }


type alias QuestionId =
    Int


type alias PageNumber =
    Int


type alias Answer =
    { id : Int
    , answer_text : String
    , is_correct : Bool
    }


type DialogType
    = Delete
    | Clear


type Msg
    = GetQuestion QuestionId
    | GetQuestionPage PageNumber
    | GetQuestionList QuestionId
    | GetMineQuestionListPage PageNumber
    | GetQuestionTagSearch PageNumber
    | ChangePage PageNumber
      -- Tags
    | TagSearchInput String
    | TagSearchAdd
    | TagSearchRemove String
    | TagSearch
    | QuestionClick Question
    | QuestionBack
      -- QuestionList (on Edit)
    | QuestionListAdd Question
    | QuestionListRemove Question
    | QuestionListHeaderInput String
    | QuestionListSave
    | QuestionListClear
    | QuestionListGenerate QuestionList
    | QuestionListDelete
    | QuestionListCancel
      -- Question List Page
    | QuestionListClick Int
    | QuestionListEdit QuestionList
      -- Filter
    | Filter Int
      -- Fetch
    | OnFetchGetQuestion (Result Http.Error Question)
    | OnFetchGetQuestionPage (Result Http.Error QuestionPage)
    | OnFetchGetQuestionFilterSearch (Result Http.Error QuestionPage)
    | OnFecthQuestionListGenerate (Result Http.Error String)
    | OnFetchSaveQuestionList (Result Http.Error Int)
    | OnFetchDeleteQuestionList (Result Http.Error String)
    | OnFetchGetMineQuestionListPage (Result Http.Error QuestionListPage)
    | OnFetchGetQuestionList (Result Http.Error QuestionList)
    | Dialog DialogType
    | NoOp
    | Snackbar (Snackbar.Msg Int)
    | Mdl (Material.Msg Msg)
