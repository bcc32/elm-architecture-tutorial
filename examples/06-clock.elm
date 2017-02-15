import Html exposing (Html)
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


type alias Model = Time


init : (Model, Cmd Msg)
init =
  (0, Cmd.none)



-- UPDATE


type Msg
  = Tick Time


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick newTime ->
      (newTime, Cmd.none)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
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


view : Model -> Html Msg
view model =
  svg [ viewBox "0 0 100 100", width "300px" ]
    [ circle [ cx "50", cy "50", r "45", fill "#0B79CE" ] []
    , hand 20 "#023963" (Time.inHours model / 24)
    , hand 30 "gray" (Time.inHours model)
    , hand 40 "red" (Time.inMinutes model)
    ]
