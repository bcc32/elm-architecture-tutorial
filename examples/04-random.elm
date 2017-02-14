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


type alias Model = (Int, Int)


init : (Model, Cmd Msg)
init =
  ((1, 1), Cmd.none)



-- UPDATE


type Msg
  = Roll
  | NewFace (Int, Int)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Roll ->
      let generator = Random.pair (Random.int 1 6) (Random.int 1 6) in
      (model, Random.generate NewFace generator)

    NewFace pair ->
      (pair, Cmd.none)



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
view (fst, snd) =
  div []
    [ h1 [] [ dieImage fst, dieImage snd ]
    , button [ onClick Roll ] [ text "Roll" ]
    ]
