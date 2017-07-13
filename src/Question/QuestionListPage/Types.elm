module Question.QuestionListPage.Types exposing (..)

import Http
import Material
import Material.Snackbar as Snackbar
import Question.QuestionList.Types as QuestionList


type alias Model =
    { questionLists : List QuestionList.QuestionList
    , questionList : QuestionList.Model
    , -- flags
      loading : Bool
    , error : String
    , -- MDL
      mdl : Material.Model
    }



-- type alias QuestionListPage =
--     { count : Int
--     , actual : Int
--     , next : Maybe String
--     , previous : Maybe String
--     , questionLists : List QuestionList.QuestionList
--     }


type Msg
    = GetMineQuestionListPage
    | QuestionListClick QuestionList.QuestionList
    | QuestionListMsg QuestionList.Msg
      -- Fetch
    | OnFetchGetMineQuestionListPage (Result Http.Error (List QuestionList.QuestionList))
    | NoOp
    | Mdl (Material.Msg Msg)
