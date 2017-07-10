module Question.QuestionListPage.View exposing (..)

import Date
import Html exposing (..)
import Html.Events exposing (..)
import Material.List as Lists
import Material.Options as Options exposing (css)


-- My modules

import Question.QuestionList.Types as QuestionList
import Question.QuestionList.Types as QuestionList
import Question.QuestionListPage.Types exposing (..)
import Utils.StringUtils as StringUtils


view : Model -> Html Msg
view model =
    div []
        [ Lists.ul [] <| List.map (questionListItems model) model.questionLists
        ]


questionListItems : Model -> QuestionList.QuestionList -> Html Msg
questionListItems model questionList =
    let
        createdDate =
            (Date.fromString questionList.create_date)
    in
        Lists.li [ Lists.withSubtitle ]
            -- ! Required on every Lists.li containing subtitle.
            [ Lists.content
                [ Options.attribute <| Html.Events.onClick (QuestionListClick questionList)
                , Options.css "cursor" "pointer"
                , Options.css "cursor" "hand"
                ]
                [ text questionList.question_list_header
                , Lists.subtitle [] [ text <| String.concat [ "Lista criada em: ", StringUtils.dateToString createdDate, " às ", StringUtils.timeToString createdDate ] ]
                , Lists.subtitle [] [ text <| String.concat [ toString questionList.question_count, " questões." ] ]
                ]
              -- , Lists.content2 []
              --     [ Toggles.checkbox Mdl
              --         [ 4 ]
              --         model.mdl
              --         [ Toggles.value 4
              --            , Options.onToggle SelectQuestionList
              --         ]
              --         []
              --     ]
            ]
