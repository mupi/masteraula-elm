port module App.Ports exposing (displayDialog, setLocalStorage, setLocalStorageQuestionList)

import Json.Encode as Encode


-- My Modules

import App.Types exposing (..)
import App.Rest exposing (..)
import Question.QuestionList.Types as QuestionList
import Question.QuestionList.Rest as QuestionList


port displayDialog : String -> Cmd msg


port portLocalStorage : Encode.Value -> Cmd msg


port portLocalStorageQuestionList : Encode.Value -> Cmd msg


setLocalStorage : LocalStorage -> Cmd msg
setLocalStorage localStorage =
    portLocalStorage <| localStorageEncoder localStorage


setLocalStorageQuestionList : QuestionList.QuestionList -> Cmd msg
setLocalStorageQuestionList questionList =
    portLocalStorageQuestionList <| QuestionList.questionListEncoder questionList
