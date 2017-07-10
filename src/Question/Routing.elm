module Question.Routing exposing (..)

import Question.Question.Types as Question


type Route
    = QuestionRoute Question.QuestionId
    | QuestionPageRoute Int
    | QuestionListRoute
    | SelectedQuestionListRoute Question.QuestionId
    | MineQuestionListsRoute
    | QuestionTagSearchRoute Int
