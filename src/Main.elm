
module Main exposing (..)


import Browser
import Html exposing (Html, div, input, span, text)
import Html.Attributes exposing (placeholder, style, value)
import Html.Events exposing (onInput)
import String


-- MAIN

main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
  { expense_report : List (Int)
  }


init : Model
init =
  { expense_report =
    [ 1721
    , 979
    , 366
    , 299
    , 675
    , 1456]
  }


-- UPDATE


type Msg
  = Change String


update : Msg -> Model -> Model
update msg model =
  model


-- VIEW

view : Model -> Html Msg
view model =
  div
  []
  ([ div [] [ text "Expense report" ]
  ]
  ++ viewList model.expense_report
  ++ viewAnswer model
  )

viewList : List (Int) -> List (Html msg)
viewList list =
    [ div
      [ style "border" "1px solid black"
      , style "display" "inline-block"
      , style "padding" "10 px"
      ]
      (list
        |> List.map (\i -> div [] [ text (String.fromInt i) ]))
    ]

viewAnswer : Model -> List (Html msg)
viewAnswer model =
    let
        answer = findAnswer model
    in
        []

findAnswer model =
