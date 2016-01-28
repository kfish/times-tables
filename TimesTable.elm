module TimesTable where

import Effects exposing (..)
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)

import Task exposing (Task)
import TaskTutorial exposing (print)
import Time exposing (second, Time)

import StartApp exposing (start)

import Datastructures.Queue as Queue

----------------------------------------------------------------------
-- Model
--

type alias Model =
  { currentBase : Int
  , sayQueue : Queue.Queue String
  , sayNow : String
  }

init : Model
init = Model 8 Queue.init "Foo"

----------------------------------------------------------------------
-- Update
--

type Action = Increment | Decrement | SpeechDone

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    Increment -> (setBase (model.currentBase + 1) model, Effects.none)
    Decrement -> (setBase (model.currentBase - 1) model, Effects.none)
    SpeechDone ->
      let
        (top, q) = Queue.dequeue model.sayQueue
        msg = case top of
          Nothing -> ""
          Just s  -> s
      in
        (Model model.currentBase q msg, Effects.none)

setBase : Int -> Model -> Model
setBase base model =
  let
    tt = timesTable base
    -- q = List.foldl Queue.enqueue model.sayQueue tt
    q = List.foldl Queue.enqueue Queue.init tt
  in
    Model base q model.sayNow

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
    , timesTableView model
    ]

timesTable : Int -> List String
timesTable base =
  let mul x y = toString x ++ " times " ++ toString y ++ " is " ++ toString (x*y)
  in List.map (mul base) [1..20]

timesTableView : Model -> Html
timesTableView model =
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
-- clock : Signal Time
-- clock =
--   Time.every (5 * second)

ticker : Signal Action
ticker = Signal.map (always SpeechDone) (Time.every (5 * second))

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
-- port smud = Signal.map (toString << .currentBase) app.model
port smud = Signal.filter (\s -> s /= "") "" (Signal.map .sayNow app.model)

port baseValue : Signal Int
port baseValue = Signal.dropRepeats (Signal.map .currentBase app.model)

-- Events from JS
port reset : Signal ()

speechDone : Signal Action
speechDone = Signal.map (always SpeechDone) reset

----------------------------------------------------------------------
-- Main
--

app =
  start
    { init = ( init, Effects.none )
    , update = update
    , view = view
    , inputs = [speechDone]
    }

main = app.html
