import Html exposing (..)
import Html.App as App
import Html.Events exposing (onInput, onClick, onBlur)
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
  , isBeingEdited : Bool
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
  | ToggleAll Bool
  | ToggleEditor Todo
  | Edit Todo String
  | ClearCompleted

update : Msg -> Model -> Model
update msg model =
  case msg of
    Input todo ->
      { model | input = todo }
    AddTodo ->
      Model "" (Todo model.currentId model.input False False :: model.todos) (model.currentId + 1) All
    RemoveTodo id ->
      { model | todos = removeTodo id model.todos }
    ToggleCompleted todo ->
      { model | todos =
        { todo | isCompleted = not todo.isCompleted } :: removeTodo todo.id model.todos }
    Filter newVisibility ->
      { model | visibility = newVisibility }
    ToggleAll bool ->
      { model | todos = List.map (\t -> { t | isCompleted = bool }) model.todos }
    ToggleEditor todo ->
      { model | todos =
        { todo | isBeingEdited = not todo.isBeingEdited } :: removeTodo todo.id model.todos }
    Edit todo newText ->
      { model | todos =
        { todo | text = newText } :: removeTodo todo.id model.todos }
    ClearCompleted ->
      { model | todos = List.filter (filterCompleted True) model.todos }
      
view : Model -> Html Msg
view model =
  div []
    [ input [ type' "text", value model.input, onInput Input ] []
    , button [ onClick AddTodo ] [ text "Add" ]
    , input
      [ type' "checkbox"
      , checked (List.length model.todos > 0 && isAllCompleted model.todos)
      , onClick (ToggleAll (isAllCompleted model.todos |> not))
      , disabled (List.length model.todos == 0)
      ]
      []
    , filterVisibility model.visibility model.todos
      |> createTodoList
    , createCounter model.todos
    , createClearButton model.todos
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

createClearButton : List Todo -> Html Msg
createClearButton todos =
  let
    isDisabled =
      not ((List.filter (filterCompleted False) todos |> List.length) > 0)
  in
    button [ disabled isDisabled, onClick ClearCompleted ] [ text "Clear completed" ]

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
    [ createTodoEditor todo
    , button
      [ onClick (ToggleEditor todo) ]
      [ (\b -> if b == True then "OK" else "Edit") todo.isBeingEdited
        |> text
      ]
    , button [ onClick (RemoveTodo todo.id) ] [ text "✕" ]
    ]

createTodoEditor : Todo -> Html Msg
createTodoEditor todo =
  if todo.isBeingEdited == True then
    input
      [ id "editor"
      , type' "text"
      , value todo.text
      , onInput (Edit todo)
      , onBlur (ToggleEditor todo)
      ]
      []
  else
    label []
      [ input
        [ type' "checkbox"
        , checked todo.isCompleted
        , onClick (ToggleCompleted todo) 
        ]
        []
      , text todo.text
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

isAllCompleted : List Todo -> Bool
isAllCompleted todos =
  List.length todos == (List.filter (filterCompleted False) todos |> List.length)

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
