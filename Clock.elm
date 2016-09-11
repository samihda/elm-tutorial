import Html exposing (Html, div, button)
import Html.App as App
import Html.Events exposing (onClick)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time, second)

main : Program Never
main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

type alias Model =
  { time : Time
  , isPaused : Bool
  }

init : (Model, Cmd Msg)
init =
  (Model 0 False, Cmd.none)

type Msg
  = Tick Time
  | Toggle

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick newTime ->
      (Model newTime False, Cmd.none)
    Toggle ->
      (Model model.time (not model.isPaused), Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  if model.isPaused then
    Sub.none
  else
    Time.every second Tick

view : Model -> Html Msg
view model =
  let
    angle =
      turns (Time.inMinutes model.time)
    handX =
      toString (50 + 40 * cos angle)
    handY =
      toString (50 + 40 * sin angle)
  in
    div []
      [ svg [ viewBox "0 0 100 100", width "300px" ]
        [ circle [ cx "50", cy "50", r "45", fill "#0B79CE" ] []
        , line [ x1 "50", y1 "50", x2 handX, y2 handY, stroke "#023963" ] []
        ]
      , button [ onClick Toggle ] [ text (toggleLabel model.isPaused) ]
      ]

toggleLabel : Bool -> String
toggleLabel bool =
  if bool == True then
    "Continue"
  else
    "Pause"
