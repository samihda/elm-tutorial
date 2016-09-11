import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import String
import Regex

main : Program Never
main =
  App.beginnerProgram { model = model, view = view, update = update }

type alias Model =
  { name : String
  , password : String
  , passwordAgain : String
  , age : String
  , validation : Bool
  }

model : Model
model =
  Model "" "" "" "" False

type Msg
  = Name String
  | Password String
  | PasswordAgain String
  | Age String
  | Submit

update : Msg -> Model -> Model
update msg model =
  case msg of
    Name name ->
      { model | name = name }
    Password password ->
      { model | password = password }
    PasswordAgain password ->
      { model | passwordAgain = password }
    Age age ->
      { model | age = age }
    Submit ->
      { model | validation = True }

view : Model -> Html Msg
view model =
  div []
    [ input [ type' "text", placeholder "Name", onInput Name ] []
    , createPasswordInput "Password" Password
    , createPasswordInput "Re-enter password" PasswordAgain
    , input [ type' "text", placeholder "Age", onInput Age ] []
    , input [ type' "submit", value "Submit", onClick Submit ] []
    , div [ style [("display", showValidation model.validation)] ]
      [ viewValidation model
      , ageValidation model.age
      ]
    ]

createPasswordInput : String -> (String -> msg) -> Html msg
createPasswordInput placeholderText msg =
  input [ type' "password", placeholder placeholderText, onInput msg ] []

showValidation : Bool -> String
showValidation validation =
  if validation then
    "block"
  else
    "none"
    
viewValidation : Model -> Html msg
viewValidation model =
  let
    (color, message) =
      if Regex.contains (Regex.regex "(?=.*\\d)(?=.*[A-Z])(?=.*[a-z])") model.password == False then
        ("red", "Password must contain number, uppercase, and lowercase characters!")
      else if String.length model.password < 8 then
        ("red", "Password is too short!")
      else if model.password /= model.passwordAgain then
        ("red", "Passwords don't match!")
      else
        ("green", "OK")
  in
    div [ style [("color", color)] ] [ text message ]

ageValidation : String -> Html msg
ageValidation age =
  let
    (color, message) =
      if Regex.contains (Regex.regex "^[1-9]+(\\d)*$") age == False then
        ("red", "Age is not a number!")
      else
        ("green", "OK")
  in
    div [ style [("color", color)] ] [ text message ]
  