import Html exposing (..)
import Html.App as App
import Html.Events exposing (onClick, on, targetValue)
import Html.Attributes exposing (src, type', placeholder, value)
import Http
import Json.Decode as Json
import Task

main : Program Never
main =
  App.program
    { init = init "cat"
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

type alias Model =
  { topic : String
  , gifUrl : String
  , status : String
  }

init : String -> (Model, Cmd Msg)
init topic =
  ( Model
    topic
    -- loading gif from https://github.com/balazsdano/elm-architecture-examples:
    "https://raw.githubusercontent.com/balazsdano/elm-architecture-examples/master/waiting.gif" 
    "OK"
  , Cmd.none
  )

type Msg
  = Topic String
  | MorePlease
  | FetchSucceed String
  | FetchFail Http.Error

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Topic newTopic ->
      (Model newTopic model.gifUrl model.status, Cmd.none)
    MorePlease ->
      (Model model.topic model.gifUrl "Please wait...", getRandomGif model.topic)
    FetchSucceed newUrl ->
      (Model model.topic newUrl "OK", Cmd.none)
    FetchFail error ->
      case error of
        Http.Timeout ->
          (Model model.topic model.gifUrl "timeout error", Cmd.none)
        Http.NetworkError ->
          (Model model.topic model.gifUrl "network error", Cmd.none)
        Http.UnexpectedPayload message ->
          (Model model.topic model.gifUrl message, Cmd.none)
        Http.BadResponse statusCode message ->
          (Model model.topic model.gifUrl ("error " ++ toString statusCode ++ ": " ++ message), Cmd.none)

getRandomGif : String -> Cmd Msg
getRandomGif topic =
  let
    url =
      "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=" ++ topic
  in
    Task.perform FetchFail FetchSucceed (Http.get decodeGifUrl url)

decodeGifUrl : Json.Decoder String
decodeGifUrl =
  Json.at ["data", "image_url"] Json.string

view : Model -> Html Msg
view model =
  div []
    [ select [ onChange Topic ]
      [ option [ value "cat" ] [ text "Cat" ]
      , option [ value "dog" ] [ text "Dog" ]
      , option [ value "meerkat" ] [ text "Meerkat" ]
      ]
    -- , input [ type' "text", placeholder "cats", onInput Topic ] []
    , button [ onClick MorePlease ] [ text "More please!" ]
    , br [] []
    , h1 [] [ text model.topic ]
    , pre [] [ text ("Status: " ++ model.status) ]
    , img [ src model.gifUrl ] []
    ]

onChange : (String -> msg) -> Attribute msg
onChange msg =
  on "change" (Json.map msg targetValue)

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none
