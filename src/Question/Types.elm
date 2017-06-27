module Question.Types exposing (..)

import Http
import Material
import Material.Snackbar as Snackbar
import Question.Question.Types as Question
import Question.QuestionList.Types as QuestionList


type alias Model =
    { questionModel : Question.Model
    , questionListModel : QuestionList.Model
    , question : Question.Question
    , questionPage : QuestionPage
    , questionListEdit : QuestionList.QuestionList
    , questionListSelected : QuestionList.QuestionList
    , mineQuestionLists : List QuestionList.QuestionList
    , -- Search
      currentTag : String
    , filters : Filter
    , subjects : List Question.Subject
    , -- List docx file generation
      generateAfterSave : Bool
    , generateWithAnswer : Bool
    , generateWithResolution : Bool
    , -- DrawerControl
      selectingQuestions : Bool
    , -- flags
      redirected : Bool
    , loading : Bool
    , downloading : Bool
    , error : String
    , -- MDL
      dialog : DialogType
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
    }


type alias QuestionPage =
    { count : Int
    , actual : Int
    , next : Maybe String
    , previous : Maybe String
    , questions : List Question.Question
    }


type alias QuestionListPage =
    { count : Int
    , actual : Int
    , next : Maybe String
    , previous : Maybe String
    , questionLists : List QuestionList.QuestionList
    }


type DialogType
    = Delete
    | Clear
    | GenerateList QuestionList.QuestionList


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


type DrawerLink
    = MineLists
    | SelectQuestions
    | SelectedQuestions


type QuestionButtonType
    = AddQuestionButton
    | RemoveQuestionButton
    | NoneQuestionButton


type Msg
    = QuestionMsg Question.Msg
    | QuestionListMsg QuestionList.Msg
      -- | GetQuestion Question.QuestionId
      -- | GetQuestionListTest QuestionList.Msg
    | GetQuestionPage Int
    | GetQuestionPageSearch Int
    | GetMineQuestionListPage Int
      -- | GetQuestionList Question.QuestionId
    | DrawerLinkClick DrawerLink
    | ChangePage Int
      -- Tags
    | TagSearchInput String
    | TagSearchAdd
    | TagSearchRemove String
    | TagSearch
    | QuestionClick Question.Question
    | QuestionBack
      -- QuestionList (on Edit)
      -- | QuestionListAdd Question.Question
      -- | QuestionListRemove Question.Question
      -- | QuestionListHeaderInput String
      -- | QuestionListSave
    | QuestionListClear
    | QuestionListGenerate QuestionList.QuestionList
      -- | QuestionListDelete
    | QuestionListCancel
      -- Question List Page
    | QuestionListClick Int
    | QuestionListEdit QuestionList.QuestionList
    | ToggleGenerateWithAnswer
    | ToggleGenerateWithResolution
      -- Filter
    | FilterLevel LevelFilterType
    | FilterSubject SubjectFilterType
    | FilterEducationLevel EducationFilterType
    | ToggleFilter ToggleFilterType
      -- Fetch
      -- | OnFetchGetQuestion (Result Http.Error Question.Question)
    | OnFetchGetQuestionPage (Result Http.Error QuestionPage)
    | OnFetchGetQuestionFilterSearch (Result Http.Error QuestionPage)
    | OnFecthQuestionListGenerate (Result Http.Error String)
      -- | OnFetchSaveQuestionList (Result Http.Error Int)
      -- | OnFetchDeleteQuestionList (Result Http.Error String)
    | OnFetchGetMineQuestionListPage (Result Http.Error (List QuestionList.QuestionList))
      -- | OnFetchGetQuestionList (Result Http.Error QuestionList.QuestionList)
    | OnFetchGetSubjects (Result Http.Error (List Question.Subject))
    | Dialog DialogType
    | NoOp
    | Snackbar (Snackbar.Msg Int)
    | Mdl (Material.Msg Msg)
