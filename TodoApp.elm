import Html exposing (..)
import Html.App as App
import Html.Events exposing (onInput, onClick)
import Html.Attributes exposing (..)

main : Program Never
main =
  App.beginnerProgram
    { model = model
    , view = view
    , update = update
    }

type alias Todo =
  { id : Int
  , text : String
  , isCompleted : Bool
  }
  
type alias Model =
  { input : String
  , todos : List Todo
  , currentId : Int
  }

model : Model
model =
  Model "" [] 1

type Msg
  = Input String
  | AddTodo
  | RemoveTodo Int
  | ToggleCompleted Todo

update : Msg -> Model -> Model
update msg model =
  case msg of
    Input todo ->
      { model | input = todo }
    AddTodo ->
      Model "" (Todo model.currentId model.input False :: model.todos) (model.currentId + 1)
    RemoveTodo id ->
      { model | todos = removeTodo id model.todos }
    ToggleCompleted todo ->
      { model | todos =
        (Todo todo.id todo.text (not todo.isCompleted)) :: (removeTodo todo.id model.todos) }
      

view : Model -> Html Msg
view model =
  div []
    [ input [ type' "text", value model.input, onInput Input ] []
    , button [ onClick AddTodo ] [ text "Add" ]
    , todoList model.todos
    ]

todoList : List Todo -> Html Msg
todoList todos =
  let
    list = List.map todoItem (List.sortBy .id todos)
  in
    ul [] list

todoItem : Todo -> Html Msg
todoItem todo =
  li
    [ style [ ("text-decoration", markFinished todo.isCompleted) ]
    ]
    [ text todo.text
    , button [ onClick (ToggleCompleted todo) ] [ text "Toggle" ]
    , button [ onClick (RemoveTodo todo.id) ] [ text "X" ] 
    ]

removeTodo : Int -> List Todo -> List Todo
removeTodo id list =
  List.filter (\todo -> todo.id /= id) list

markFinished : Bool -> String
markFinished bool =
  if bool == True then
    "line-through"
  else
    "none"
