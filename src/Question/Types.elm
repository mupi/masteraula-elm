module Question.Types exposing (..)

import Material
import Question.QuestionListEdit.Types as QuestionListEdit
import Question.SelectedQuestion.Types as SelectedQuestion
import Question.SelectedQuestionList.Types as SelectedQuestionList
import Question.QuestionListPage.Types as QuestionListPage
import Question.QuestionPage.Types as QuestionPage
import Question.Routing exposing (..)


type alias Model =
    { questionListEdit : QuestionListEdit.Model
    , selectedQuestion : SelectedQuestion.Model
    , selectedQuestionList : SelectedQuestionList.Model
    , questionListPage : QuestionListPage.Model
    , questionPage : QuestionPage.Model
    , route : Route
    , -- flags
      redirected : Bool
    , error : String
    , -- MDL
      mdl : Material.Model
    }


type DrawerLink
    = MineLists
    | SelectQuestions
    | SelectedQuestions


type Msg
    = QuestionListEditMsg QuestionListEdit.Msg
    | SelectedQuestionMsg SelectedQuestion.Msg
    | SelectedQuestionListMsg SelectedQuestionList.Msg
    | QuestionListPageMsg QuestionListPage.Msg
    | QuestionPageMsg QuestionPage.Msg
    | DrawerLinkClick DrawerLink
    | OnLocationChange Route
    | NoOp
    | Mdl (Material.Msg Msg)
