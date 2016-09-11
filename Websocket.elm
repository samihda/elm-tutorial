import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import WebSocket

main : Program Never
main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

type alias Model =
  { input : String
  , messages : List String
  }

init : (Model, Cmd Msg)
init =
  (Model "" [], Cmd.none)

type Msg
  = Input String
  | Send
  | NewMessage String

update : Msg -> Model -> (Model, Cmd Msg)
update msg { input, messages } =
  case msg of
    Input newInput ->
      (Model newInput messages, Cmd.none)
    Send ->
      (Model "" messages, WebSocket.send "ws://echo.websocket.org" input)
    NewMessage str ->
      (Model input (str :: messages), Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  WebSocket.listen "ws://echo.websocket.org" NewMessage

view : Model -> Html Msg
view model =
  div []
    [ input [ value model.input, onInput Input ] []
    , button [ onClick Send ] [ text "Send" ]
    , div [] (List.map viewMessage model.messages)
    ]

viewMessage : String -> Html msg
viewMessage msg =
  div [] [ text msg ]
