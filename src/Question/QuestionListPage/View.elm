module Question.QuestionListPage.View exposing (..)

import Date
import Html exposing (..)
import Html.Events exposing (..)
import Material.List as Lists
import Material.Spinner as Loading
import Material.Options as Options exposing (css)


-- My modules

import Question.QuestionList.Types as QuestionList
import Question.QuestionList.Types as QuestionList
import Question.QuestionListPage.Types exposing (..)
import Utils.StringUtils as StringUtils


view : Model -> Html Msg
view model =
    if model.loading then
        Options.div [ Options.cs "question_loader_div" ] [ Options.div [ Options.cs "question_loader" ] [ Loading.spinner [ Loading.active model.loading ] ] ]
    else
        Options.div []
            [ Lists.ul [] <| List.map (questionListItems model) model.questionLists
            ]


questionListItems : Model -> QuestionList.QuestionList -> Html Msg
questionListItems model questionList =
    let
        createdDate =
            (Date.fromString questionList.create_date)
    in
        Lists.li [ Lists.withSubtitle ]
            [ Lists.content
                [ Options.attribute <| Html.Events.onClick (QuestionListClick questionList)
                , Options.css "cursor" "pointer"
                , Options.css "cursor" "hand"
                ]
                [ text questionList.question_list_header
                , Lists.subtitle [] [ text <| String.concat [ "Lista criada em: ", StringUtils.dateToString createdDate, " às ", StringUtils.timeToString createdDate ] ]
                , Lists.subtitle [] [ text <| String.concat [ toString questionList.question_count, " questões." ] ]
                ]
            ]
