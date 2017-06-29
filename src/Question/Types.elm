module Question.Types exposing (..)

import Material
import Question.QuestionListEdit.Types as QuestionListEdit
import Question.Routing exposing (..)


type alias Model =
    { questionListEdit : QuestionListEdit.Model
    , route : Route
    , -- flags
      redirected : Bool
    , loading : Bool
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
    | DrawerLinkClick DrawerLink
    | OnLocationChange Route
    | NoOp
    | Mdl (Material.Msg Msg)
