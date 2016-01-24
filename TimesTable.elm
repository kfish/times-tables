module TimesTable where

import Effects exposing (..)
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)

import Task exposing (Task) import TaskTutorial exposing (print)
import Time exposing (second, Time)

import StartApp exposing (start)

import Datastructures.Queue

----------------------------------------------------------------------
-- Model
--

type alias Model =
  { currentBase : Int
  }

----------------------------------------------------------------------
-- Update
--

type Action = Increment | Decrement

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    Increment -> (Model (model.currentBase + 1), Effects.none)
    Decrement -> (Model (model.currentBase - 1), Effects.none)

----------------------------------------------------------------------
-- View
--

view : Signal.Address Action -> Model -> Html
view address model =
  div []
    [ div []
      [ button [ onClick address Decrement ] [ text "-" ]
      , div [countStyle] [ text (toString model.currentBase) ]
      , button [ onClick address Increment ] [ text "+" ]
      ]
    , timesTable model
    ]

timesTable : Model -> Html
timesTable model =
  let mul x y = toString x ++ " times " ++ toString y ++ " is " ++ toString (x*y)
  in
  ul []
    (List.map (\y -> li [] [text (mul (model.currentBase) y)]) [1..20])
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

----------------------------------------------------------------------
-- Ports
--

-- A signal that updates to the current time every second
clock : Signal Time
clock =
  Time.every second

{-
-- Turn the clock into a signal of tasks
printTasks : Signal (Task x ())
printTasks =
  Signal.map print clock

-- Actually perform all those tasks
port runner : Signal (Task x ())
port runner =
  printTasks
-}

port smud : Signal String
-- port smud = Signal.map toString clock
port smud = Signal.map (toString << .currentBase) app.model

----------------------------------------------------------------------
-- Main
--

app =
  start
    { init = ( Model 8, Effects.none )
    , update = update
    , view = view
    , inputs = []
    }

main = app.html
