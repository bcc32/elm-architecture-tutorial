import Html exposing (..)
import Html.Events exposing (..)
import Random
import Char



main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL


type alias Model =
  { dieFace : Int
  }


init : (Model, Cmd Msg)
init =
  (Model 1, Cmd.none)



-- UPDATE


type Msg
  = Roll
  | NewFace Int


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Roll ->
      (model, Random.generate NewFace (Random.int 1 6))

    NewFace newFace ->
      (Model newFace, Cmd.none)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW


dieImage : Int -> Html Msg
dieImage n =
  -- unicode die face characters
  let code = n + 9855 in
  text (String.fromChar (Char.fromCode code))


view : Model -> Html Msg
view model =
  div []
    [ h1 [] [ dieImage model.dieFace ]
    , button [ onClick Roll ] [ text "Roll" ]
    ]
