import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Http
import Json.Decode exposing (..)
import Task

main : Program Never
main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

type alias Model =
  List String

init : (Model, Cmd Msg)
init = ([], getSuggestions) 

type Msg
  = FetchSucceed (List String)
  | FetchFail Http.Error

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    FetchSucceed suggestions ->
      (suggestions, Cmd.none)
    FetchFail _ ->
      (model, Cmd.none)
  
view : Model -> Html a
view model =
  div []
    (List.map (\url -> img [ src url, width 50, height 50 ] []) model)

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

getSuggestions : Cmd Msg
getSuggestions =
  Task.perform FetchFail FetchSucceed (Http.get decode "https://api.github.com/users")

decode : Decoder (List String)
decode =
  Json.Decode.list ("avatar_url" := string)
