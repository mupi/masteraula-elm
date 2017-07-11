module Question.QuestionPage.Types exposing (..)

import Http
import Material
import Material.Snackbar as Snackbar
import Question.Question.Types as Question
import Question.QuestionList.Types as QuestionList


type alias Model =
    { questionPage : QuestionPage
    , questionList : QuestionList.Model
    , -- Search
      filters : Filter
    , subjects : List Question.Subject
    , -- MDL
      error : String
    , snackbar : Snackbar.Model Int
    , mdl : Material.Model
    }


type alias Filter =
    { levelFilters : List LevelFilterType
    , levelToggle : Bool
    , subjectFilters : List String
    , subjectToggle : Bool
    , educationLevelFilters : List String
    , educationToggle : Bool
    , tags : List String
    , currentTag : String
    }


type alias QuestionPage =
    { count : Int
    , actual : Int
    , next : Maybe String
    , previous : Maybe String
    , questions : List Question.Question
    }


type LevelFilterType
    = AllLevel
    | EasyLevel
    | MediumLevel
    | HardLevel


type SubjectFilterType
    = StringSubject String
    | AllSubject


type EducationFilterType
    = StringEducationLevel String
    | AllEducationLevel


type ToggleFilterType
    = LevelToggle
    | SubjectToggle
    | EducationToggle


type QuestionButtonType
    = AddQuestionButton
    | RemoveQuestionButton
    | NoneQuestionButton


type Msg
    = QuestionListMsg QuestionList.Msg
    | GetQuestionPage Int
    | GetQuestionPageSearch Int
    | ChangePage Int
    | SelectedQuestions
    | UpdateQuestionList QuestionList.Model
      -- Tags
    | TagSearchInput String
    | TagSearchAdd
    | TagSearchRemove String
    | TagSearch
    | QuestionClick Question.Question
      -- Filter
    | FilterLevel LevelFilterType
    | FilterSubject SubjectFilterType
    | FilterEducationLevel EducationFilterType
    | ToggleFilter ToggleFilterType
      -- Fetch
    | OnFetchGetQuestionPage (Result Http.Error QuestionPage)
    | OnFetchGetQuestionFilterSearch (Result Http.Error QuestionPage)
    | OnFetchGetSubjects (Result Http.Error (List Question.Subject))
    | NoOp
    | Snackbar (Snackbar.Msg Int)
    | Mdl (Material.Msg Msg)
