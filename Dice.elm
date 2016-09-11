import Html exposing (..)
import Html.App as App
import Html.Events exposing (onClick)
import Random

main : Program Never
main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

type alias Model =
  { dieFace1 : Int
  , dieFace2 : Int
  }

view : Model -> Html Msg
view model =
  div []
    [ h1 [] [ text (toString model.dieFace1) ]
    , h1 [] [ text (toString model.dieFace2) ]
    , button [ onClick Roll ] [ text "Roll" ]
    ]

type Msg
  = Roll
  | NewFace (Int, Int)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Roll ->
      ( model
      , Random.generate NewFace
        ( Random.pair (Random.int 1 6) (Random.int 1 6)
        )
      )
    NewFace (newFace1, newFace2) ->
      (Model newFace1 newFace2, Cmd.none)

init : (Model, Cmd Msg)
init =
  (Model 1 1, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none
