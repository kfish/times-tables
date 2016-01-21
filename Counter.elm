module Counter where

import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)

----------------------------------------------------------------------
-- Model
--

type alias Model = Int

----------------------------------------------------------------------
-- Update
--

type Action = Increment | Decrement

update : Action -> Model -> Model
update action model =
  case action of
    Increment -> model + 1
    Decrement -> model - 1

----------------------------------------------------------------------
-- View
--

view : Signal.Address Action -> Model -> Html
view address model =
  div []
    [ div []
      [ button [ onClick address Decrement ] [ text "-" ]
      , div [countStyle] [ text (toString model) ]
      , button [ onClick address Increment ] [ text "+" ]
      ]
    , timesTable model
    ]

timesTable : Int -> Html
timesTable val =
  let mul x y = toString x ++ " times " ++ toString y ++ " is " ++ toString (x*y)
  in
  ul []
    (List.map (\y -> li [] [text (mul val y)]) [1..20])
    -- [ text (mul val 7) ]
  

countStyle : Attribute
countStyle =
  style
    [ ("font-size", "20px")
    , ("font-family", "monospace")
    , ("display", "inline-block")
    , ("width", "50px")
    , ("text-align", "center")
    ]
