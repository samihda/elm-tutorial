import Draggable exposing (Draggable)
import Html exposing (Html, div)
import Html.App as App
import Mouse exposing (Position)

main : Program Never
main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

type alias Model =
  { box1 : Draggable
  , box2 : Draggable
  }

init : (Model, Cmd Msg)
init =
  ( Model
      (Draggable (Position 0 0) Nothing)
      (Draggable (Position 150 0) Nothing)
  , Cmd.none
  )

type Msg
  = Reposition1 Draggable.Msg
  | Reposition2 Draggable.Msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Reposition1 submsg ->
      ({ model | box1 = Draggable.updateHelp submsg model.box1 }, Cmd.none)
    Reposition2 submsg ->
      ({ model | box2 = Draggable.updateHelp submsg model.box2 }, Cmd.none)

view : Model -> Html Msg
view model =
  div
    []
    [ Draggable.view model.box1 |> App.map Reposition1
    , Draggable.view model.box2 |> App.map Reposition2
    ]

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Draggable.subscriptions model.box1 |> Sub.map Reposition1
    , Draggable.subscriptions model.box2 |> Sub.map Reposition2
    ]
