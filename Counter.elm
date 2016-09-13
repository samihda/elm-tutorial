import Html exposing (Html, button, div, text)
import Html.App as App
import Html.Events exposing (onClick)


main : Program Never
main =
  App.beginnerProgram { model = model, view = view, update = update }


type alias Model = Int

model : Model
model =
  0


type Msg = Increment | Decrement | Reset

update : Msg -> Model -> Model
update msg model =
  case msg of
    Increment ->
      model + 1
    
    Decrement ->
      model - 1

    Reset ->
      0


view : Model -> Html Msg
view model =
  div []
    [ div [] [ text (toString model) ]
    , button [ onClick Decrement ] [ text "-" ]
    , button [ onClick Increment ] [ text "+" ]
    , button [ onClick Reset ] [ text "Reset" ]
    ]
