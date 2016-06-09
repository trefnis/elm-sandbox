module App.SearchUserBar.SearchUserBar exposing (Model, Msg, init, update, view, subscriptions) --where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick, onSubmit, onFocus, onBlur)
import String
import Mouse
import Json.Decode as Json exposing ((:=))

import App.Api.Api as Api

-- MODEL

type alias Model =
  { textInput : String
  , searchResults : Maybe (List SearchResult)
  , isSearching : Bool
  , isError : Bool
  , shouldExpand : Bool
  , hasJustReceivedFocus : Bool
  }

type alias SearchResult = 
  { userLogin : String
  , avatarUrl : String 
  }

init : Model
init =
  { textInput = ""
  , searchResults = Nothing
  , isSearching = False
  , isError = False
  , shouldExpand = False
  , hasJustReceivedFocus = False
  }


-- UPDATE

type Msg
  = GotFocus
  | CustomClick Mouse.Position
  | LostFocus
  | TextInput String
  | Submit
  | SearchStatus (Api.SearchStatus SearchResult)
  | ResultSelected SearchResult

type alias SelectedLogin = Maybe String

update : Msg -> Model -> ( Model, Cmd Msg, SelectedLogin )
update msg model =
  case msg of
    GotFocus ->
      bareModel { model | shouldExpand = True, hasJustReceivedFocus = True }

    CustomClick position ->
      if model.hasJustReceivedFocus then
        bareModel { model | hasJustReceivedFocus = False }
      else
        update LostFocus model

    LostFocus ->
      bareModel { model | shouldExpand = False, hasJustReceivedFocus = False }

    TextInput input ->
      let
        shouldSubmit = hasSufficientText input
        model = 
          { model | 
            textInput = input
          , shouldExpand = False
          , searchResults = Nothing
          }        
      in
        if shouldSubmit then 
          update Submit model
        else 
          bareModel model

    Submit ->
      let newModel = { model | shouldExpand = True, isSearching = True }
      in
        ( newModel, findUsers model.textInput, Nothing )

    SearchStatus (Api.SearchFailed error) ->
      bareModel { model | isError = True, isSearching = False }

    SearchStatus (Api.SearchSucceed results) ->
      let
        searchResults = Just results
        newModel = 
          { model |
            isSearching = False
          , searchResults = searchResults
          }
      in
        bareModel newModel

    ResultSelected result ->
        ( model, Cmd.none, Just result.userLogin )

bareModel : Model -> ( Model, Cmd Msg, SelectedLogin )
bareModel model =
  ( model, Cmd.none, Nothing )

hasResults model =
  case model.searchResults of
    Nothing -> False
    Just _ -> True

hasSufficientText textInput = 
  String.length textInput > 2


-- VIEW

view : Model -> Html Msg
view model =
  Html.form 
    [ onSubmit Submit, onClick GotFocus ]
    [ input 
        [ placeholder "Enter user name" 
        , onInput TextInput
        , onFocus GotFocus
        , onBlur LostFocus
        ] []
    , button [] [ text "search" ]
    , p [] [ text model.textInput ]
    , if model.isSearching then loadingIcon else text ""
    , p [] [ text <| "Expand: " ++ (toString model.shouldExpand) ]
    , p [] [ text <| "Is error: " ++ (toString model.isError) ]
    , resultsView model.searchResults
    ]

resultsView searchResults =
  case searchResults of
    Nothing -> text ""

    Just results ->
      let
        listItem = \result -> 
          li [ onClick (ResultSelected result) ] 
            [ p [] [ text result.userLogin ]
            , img 
                [ src result.avatarUrl
                , width 50
                ] []
            ]

        listItems = List.map listItem results

        list = List.take 10 listItems
      in
        ul [] list

loadingIcon =
  img [ src "../images/Loading_icon.gif", alt "loading..." ] []

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Mouse.clicks CustomClick


-- API

findUsers : String -> Cmd Msg
findUsers query =
  let find = Api.findUsers getUsersFromJson query
  in Cmd.map SearchStatus find

getUsersFromJson : Json.Decoder (List SearchResult)
getUsersFromJson =
  let
    userDecoder = Json.object2 SearchResult
      ("login" := Json.string)
      ("avatar_url" := Json.string)
  in
    Json.at ["items"] (Json.list userDecoder)