import Html exposing (..)
import Html.Events exposing (onInput, onClick)
import Html.Attributes exposing (..)
import String exposing (trim)
import Navigation

main : Program Never
main =
  Navigation.program urlParser
    { init = init
    , view = view
    , update = update
    , urlUpdate = urlUpdate
    , subscriptions = subscriptions
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
  { textInput : String
  , todos : List Todo
  , currentId : Int
  , visibility : Visibility
  }

init : String -> (Model, Cmd Msg)
init url =
  urlUpdate url (Model "" [] 1 All)

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

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let
    newModel =
      case msg of
        Input todo ->
          { model | textInput = todo }
        AddTodo ->
          Model "" (Todo model.currentId model.textInput False False :: model.todos) (model.currentId + 1) All
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
  in
    (newModel, Navigation.newUrl (toUrl newModel.visibility))

toUrl : Visibility -> String
toUrl visibility =
  let
    view =
      case visibility of
        All -> ""
        Active -> "active"
        Completed -> "completed"
  in
    "#/" ++ view

fromUrl : String -> String
fromUrl url =
  String.dropLeft 2 url

urlParser : Navigation.Parser String
urlParser =
  Navigation.makeParser (fromUrl << .hash)

urlUpdate : String -> Model -> (Model, Cmd Msg)
urlUpdate url model =
  case url of
    "active" ->
      ({ model | visibility = Active }, Cmd.none)
    "completed" ->
      ({ model | visibility = Completed }, Cmd.none)
    "" ->
      ({ model | visibility = All }, Cmd.none)
    _ ->
      ({ model | visibility = All }, Navigation.modifyUrl (toUrl All))

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none
      
view : Model -> Html Msg
view { textInput, todos, visibility } =
  div []
    [ input [ type' "text", value textInput, onInput Input ] []
    , button
      [ onClick AddTodo
      , disabled (trim textInput == "")
      ]
      [ text "Add" ]
    , input
      [ type' "checkbox"
      , checked (List.length todos > 0 && isAllCompleted todos)
      , onClick (ToggleAll (isAllCompleted todos |> not))
      , disabled (List.length todos == 0)
      ]
      []
    , createTodoList visibility todos
    , createCounter todos
    , createClearButton todos
    , div []
      [ createFilterButton "All" All (visibility == All)
      , createFilterButton "Active" Active (visibility == Active)
      , createFilterButton "Completed" Completed (visibility == Completed)
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

createTodoList : Visibility -> List Todo -> Html Msg
createTodoList visibility todos =
  let
    list = filterVisibility visibility todos
      |> List.sortBy .id
      |> List.map createTodoItem
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
      [ onClick (ToggleEditor todo)
      , disabled (trim todo.text == "")
      ]
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
    div
      []
      [ toString count
        |> (++) "To do: "
        |> text
      ]

filterCompleted : Bool -> Todo -> Bool
filterCompleted flag todo =
  if flag == True then
    not todo.isCompleted
  else
    todo.isCompleted
