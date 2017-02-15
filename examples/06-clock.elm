import Html exposing (Html, button, div)
import Html.Events exposing (onClick)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time, second)



main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL


type alias Model =
  { time : Time
  , paused : Bool
  }


init : (Model, Cmd Msg)
init =
  (Model 0 False, Cmd.none)



-- UPDATE


type Msg
  = Tick Time
  | TogglePause


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick newTime ->
      ({ model | time = newTime }, Cmd.none)
    TogglePause ->
      ({ model | paused = not model.paused }, Cmd.none)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  if model.paused then
    Sub.none
  else
    Time.every (second / 6) Tick



-- VIEW


hand : Float -> String -> Float -> Svg Msg
hand length color numTurns =
  let
    angle = turns numTurns
    handX = toString (50 + length * sin angle)
    handY = toString (50 - length * cos angle)
  in
  line [ x1 "50", y1 "50", x2 handX, y2 handY, stroke color ] []


pauseButton : Model -> Html Msg
pauseButton model =
  button
    [ onClick TogglePause ]
    [ text (if model.paused then "Unpause" else "Pause") ]


view : Model -> Html Msg
view model =
  div []
    [ svg [ viewBox "0 0 100 100", width "300px" ]
      [ circle [ cx "50", cy "50", r "45", fill "#0B79CE" ] []
      , hand 20 "#023963" (Time.inHours model.time / 24)
      , hand 30 "gray" (Time.inHours model.time)
      , hand 40 "red" (Time.inMinutes model.time)
      ]
    , pauseButton model
    ]
