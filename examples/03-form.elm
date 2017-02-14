import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Char exposing (isDigit, isLower, isUpper)


main =
  Html.beginnerProgram
    { model = model
    , view = view
    , update = update
    }



-- MODEL


type alias Model =
  { name : String
  , password : String
  , passwordAgain : String
  , age : String
  , showValidation: Bool
  }


model : Model
model =
  Model "" "" "" "" False



-- UPDATE


type Msg
    = Name String
    | Age String
    | Password String
    | PasswordAgain String
    | Validate


update : Msg -> Model -> Model
update msg model =
  let newModel = { model | showValidation = False } in
  case msg of
    Name name ->
      { newModel | name = name }

    Age age ->
      { newModel | age = age }

    Password password ->
      { newModel | password = password }

    PasswordAgain password ->
      { newModel | passwordAgain = password }

    Validate ->
      { newModel | showValidation = True }



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ input [ type_ "text", placeholder "Name", onInput Name ] []
    , input [ type_ "text", placeholder "Age", onInput Age ] []
    , input [ type_ "password", placeholder "Password", onInput Password ] []
    , input [ type_ "password", placeholder "Re-enter Password", onInput PasswordAgain ] []
    , button [ onClick Validate ] [ text "Submit!" ]
    , viewValidation model
    ]


isInt : String -> Bool
isInt input =
  case String.toInt input of
    Ok _ -> True
    Err _ -> False


isSecurePassword : String -> Bool
isSecurePassword input =
     String.any isDigit input
  && String.any isLower input
  && String.any isUpper input


viewValidation : Model -> Html msg
viewValidation model =
  if not model.showValidation then
    text ""
  else
    let
      (color, message) =
        if not (isInt model.age) then
          ("red", "Age is not a number!")
        else if String.length model.password <= 8 then
          ("red", "Password is too short!")
        else if not (isSecurePassword model.password) then
          ("red", "Password is too simple!")
        else if model.password /= model.passwordAgain then
          ("red", "Passwords do not match!")
        else
          ("green", "OK")
    in
      div [ style [("color", color)] ] [ text message ]
