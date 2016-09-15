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

type Visibility
  = All
  | Active
  | Completed

type alias Model =
  { input : String
  , todos : List Todo
  , currentId : Int
  , visibility : Visibility
  }

model : Model
model =
  Model "" [] 1 All

type Msg
  = Input String
  | AddTodo
  | RemoveTodo Int
  | ToggleCompleted Todo
  | Filter Visibility

update : Msg -> Model -> Model
update msg model =
  case msg of
    Input todo ->
      { model | input = todo }
    AddTodo ->
      Model
        ""
        (Todo model.currentId model.input False :: model.todos)
        (model.currentId + 1)
        All
    RemoveTodo id ->
      { model | todos = removeTodo id model.todos }
    ToggleCompleted todo ->
      { model | todos =
        (Todo todo.id todo.text (not todo.isCompleted)) :: (removeTodo todo.id model.todos) }
    Filter newVisibility ->
      { model | visibility = newVisibility }
      
view : Model -> Html Msg
view model =
  div []
    [ input [ type' "text", value model.input, onInput Input ] []
    , button [ onClick AddTodo ] [ text "Add" ]
    , filterVisibility model.visibility model.todos
      |> createTodoList
    , createCounter model.todos
    , div []
      [ createFilterButton "All" All (model.visibility == All)
      , createFilterButton "Active" Active (model.visibility == Active)
      , createFilterButton "Completed" Completed (model.visibility == Completed)
      ]
    ]

filterVisibility : Visibility -> List Todo -> List Todo
filterVisibility visibility todos =
  case visibility of
    All ->
      todos
    Active ->
      List.filter (filterCompleted True) todos
    Completed ->
      List.filter (filterCompleted False) todos

createFilterButton : String -> Visibility -> Bool -> Html Msg
createFilterButton label visibility isActive =
  button [ disabled isActive, onClick (Filter visibility) ] [ text label ]

createTodoList : List Todo -> Html Msg
createTodoList todos =
  let
    list = List.map createTodoItem (List.sortBy .id todos)
  in
    ul [] list

createTodoItem : Todo -> Html Msg
createTodoItem todo =
  li
    [ style
      [ ("text-decoration", toggleCompleted todo.isCompleted)
      , ("list-style", "none")
      ]
    ]
    [ label []
      [ input
        [ type' "checkbox"
        , checked todo.isCompleted
        , onClick (ToggleCompleted todo) 
        ]
        []
      , text todo.text
      ]
    , button [ onClick (RemoveTodo todo.id) ] [ text "X" ] 
    ]

removeTodo : Int -> List Todo -> List Todo
removeTodo id list =
  List.filter (\todo -> todo.id /= id) list

toggleCompleted : Bool -> String
toggleCompleted bool =
  if bool == True then
    "line-through"
  else
    "none"

createCounter : List Todo -> Html a
createCounter todos =
  let
    count =
      List.filter (filterCompleted True) todos
        |> List.length
  in
    div [] [ text (toString count) ]

filterCompleted : Bool -> Todo -> Bool
filterCompleted flag todo =
  if flag == True then
    not todo.isCompleted
  else
    todo.isCompleted
