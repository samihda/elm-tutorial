module Draggable exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (on)
import Json.Decode as Json exposing ((:=))
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
  Draggable

type alias Draggable =
  { position : Position 
  , drag : Maybe Drag
  }

type alias Drag =
  { start : Position
  , current : Position
  }

init : (Model, Cmd Msg)
init =
  (Draggable (Position 0 0) Nothing, Cmd.none)

type Msg
  = DragStart Position
  | DragAt Position
  | DragEnd Position

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  (updateHelp msg model, Cmd.none)

updateHelp : Msg -> Model -> Model
updateHelp msg ({ position, drag } as model) =
  case msg of
    DragStart xy ->
      Draggable position (Just (Drag xy xy))
    DragAt xy ->
      Draggable position (Maybe.map (\{ start } -> Drag start xy) drag)
    DragEnd _ ->
      Draggable (getPosition model) Nothing

subscriptions : Model -> Sub Msg
subscriptions model =
  case model.drag of
    Nothing ->
      Sub.none
    Just _ ->
      Sub.batch [ Mouse.moves DragAt, Mouse.ups DragEnd ]

(=>) : a -> b -> (a, b)
(=>) = (,)

view : Model -> Html Msg
view model =
  let
    realPosition =
      getPosition model
  in
    div
      [ onMouseDown
      , style
        [ "background-color" => "#3C8D2F"
        , "cursor" => "move"
        , "width" => "100px"
        , "height" => "100px"
        , "position" => "absolute"
        , "left" => px realPosition.x
        , "top" => px realPosition.y
        , "color" => "white"
        , "display" => "flex"
        , "align-items" => "center"
        , "justify-content" => "center"
        ]
      ]
      [ text "Drag me!" ]

px : Int -> String
px number =
  toString number ++ "px"

getPosition : Model -> Position
getPosition { position, drag } =
  case drag of
    Nothing ->
      position
    Just { start, current } ->
      Position
        (position.x + current.x - start.x)
        (position.y + current.y - start.y)

onMouseDown : Attribute Msg
onMouseDown =
  on "mousedown" (Json.map DragStart Mouse.position)
