import Html exposing (..)
import Html.App as App
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)
import Http
import Json.Decode exposing (..)
import Task
import Random
import List.Extra

main : Program Never
main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

type alias User =
  { id : Int
  , name : String
  , url : String
  , avatarUrl : String
  }

type alias Model =
  { users : List User
  , suggestions : List User
  }

init : (Model, Cmd Msg)
init = (Model [] [], getRandomOffset)

type Msg
  = Request
  | Refresh Int
  | FetchSucceed (List User)
  | FetchFail Http.Error
  | Next Int
  | Dismiss Int

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Request ->
      (model, getRandomOffset)
    Refresh offset ->
      (model, getSuggestions offset)
    FetchSucceed newUsers ->
      (Model (List.drop 3 newUsers) (List.take 3 newUsers), Cmd.none)
    FetchFail _ ->
      (model, Cmd.none)
    Next index ->
      let
        newSuggestion = getSuggestion index model.users
      in
        ( Model
          (List.filter (\u -> u /= newSuggestion) model.users)
          (addNewSuggestion newSuggestion model.suggestions)
        , Cmd.none
        )
    Dismiss id ->
      ( { model | suggestions = removeSuggestion id model.suggestions }
      , List.length model.users - 1 |> getRandomIndex
      )

getSuggestions : Int -> Cmd Msg
getSuggestions offset =
  Task.perform FetchFail FetchSucceed (Http.get decode ("https://api.github.com/users?since=" ++ toString offset))

decode : Decoder (List User)
decode =
  Json.Decode.list
    (object4 User
      ("id" := int)
      ("login" := string)
      ("html_url" := string)
      ("avatar_url" := string))

getRandomOffset : Cmd Msg
getRandomOffset =
  Random.generate Refresh (Random.int 0 20000000)

getRandomIndex : Int -> Cmd Msg
getRandomIndex upper =
  if upper < 0 then
    getRandomOffset
  else
    Random.generate Next (Random.int 0 upper)

getSuggestion : Int -> List User -> User
getSuggestion id users =
  case (List.Extra.getAt id users) of
    Just user ->
      user
    Nothing ->
      (User 0 "not found" "" "")

removeSuggestion : Int -> List User -> List User
removeSuggestion suggestionId suggestions =
  List.Extra.replaceIf
    (\u -> u.id == suggestionId)
    (User 0 "" "" "")
    suggestions

addNewSuggestion : User -> List User -> List User
addNewSuggestion newSuggestion suggestions =
  List.Extra.replaceIf
    (\u -> u.id == 0)
    newSuggestion
    suggestions

view : Model -> Html Msg
view { suggestions } =
  div []
    [ button [ onClick Request ] [ text "Refresh" ]
    , createList suggestions
    ]

createList : List User -> Html Msg
createList users =
  List.map createListItem users
    |> ul []

createListItem : User -> Html Msg
createListItem { id, name, url, avatarUrl } =
  li []
    [ div []
      [ img [ src avatarUrl, width 50, height 50 ] []
      , a [ href url ] [ text name ]
      , button [ onClick (Dismiss id) ] [ text "✕" ]
      ]
    ]

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none
