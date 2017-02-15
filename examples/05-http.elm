import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode



main =
  Html.program
    { init = init "cats"
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL


type alias Model =
  { topic : String
  , gifUrl : String
  , error : Maybe Http.Error
  }


init : String -> (Model, Cmd Msg)
init topic =
  ( Model topic "waiting.gif" Nothing
  , getRandomGif topic
  )



-- UPDATE


type Msg
  = MorePlease
  | NewGif (Result Http.Error String)
  | NewTopic String


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    MorePlease ->
      ({ model | error = Nothing }, getRandomGif model.topic)

    NewGif (Ok newUrl) ->
      (Model model.topic newUrl Nothing, Cmd.none)

    NewGif (Err e) ->
      ({ model | error = Just e }, Cmd.none)

    NewTopic t ->
      { model | topic = t }
      |> update MorePlease



-- VIEW


maybeError : Maybe Http.Error -> Html Msg
maybeError me =
  case me of
    Nothing -> div [ hidden True ] []
    Just e ->
      let errorText =
        case e of
          Http.BadUrl u -> "Bad url " ++ u
          Http.Timeout -> "Timeout"
          Http.NetworkError -> "Network Error"
          Http.BadStatus response ->
            "Bad status: "
              ++ toString response.status.code
              ++ " "
              ++ response.status.message
          Http.BadPayload s _ -> "Bad payload: " ++ s
      in
      div [] [ text errorText ]


view : Model -> Html Msg
view model =
  div []
    [ h2 [] [text model.topic]
    , input [ placeholder "Topic", onInput NewTopic ] []
    , button [ onClick MorePlease ] [ text "More Please!" ]
    , br [] []
    , maybeError model.error
    , img [src model.gifUrl] []
    ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- HTTP


getRandomGif : String -> Cmd Msg
getRandomGif topic =
  let
    url =
      "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=" ++ topic
  in
    Http.send NewGif (Http.get url decodeGifUrl)


decodeGifUrl : Decode.Decoder String
decodeGifUrl =
  Decode.at ["data", "image_url"] Decode.string
