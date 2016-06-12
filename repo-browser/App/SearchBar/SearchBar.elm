port module App.SearchBar.SearchBar exposing (Model, Msg(SearchStatus, ResultSelected), init, update, view, subscriptions) --where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick, onSubmit, onFocus, onBlur, onWithOptions, on)
import String
import Mouse
import App.Api.Api as Api
import Json.Decode exposing ((:=))
import Json.Encode

-- MODEL

type alias Model searchResult =
  { textInput : String
  , searchResults : Maybe (List searchResult)
  , isSearching : Bool
  , isError : Bool
  , shouldExpand : Bool
  }

init : Model searchResult
init =
  { textInput = ""
  , searchResults = Nothing
  , isSearching = False
  , isError = False
  , shouldExpand = False
  }


-- UPDATE

type Msg searchResult
  = GotFocus Json.Decode.Value
  | ClickInside
  | ClickOutside Mouse.Position
  | LostFocus
  | TextInput String
  | Submit
  | SearchStatus (Api.Status (List searchResult))
  | ResultSelected searchResult

update 
  : (String -> Cmd (Msg result))
  -> Msg result
  -> Model result
  -> ( Model result, Cmd (Msg result), Maybe result )

update find msg model =
  case msg of
    GotFocus event ->
      ( { model | shouldExpand = hasResults model }, selectText event, Nothing )

    ClickInside ->
      bareModel model

    ClickOutside position ->
      update find LostFocus model

    LostFocus ->
      bareModel { model | shouldExpand = False }

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
          update find Submit model
        else 
          bareModel model

    Submit ->
      let newModel = { model | shouldExpand = True, isSearching = True }
      in
        ( newModel, find model.textInput, Nothing )

    SearchStatus (Api.FetchFailed error) ->
      bareModel { model | isError = True, isSearching = False }

    SearchStatus (Api.FetchSucceed results) ->
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
        ( model, Cmd.none, Just result )

bareModel : Model result -> ( Model result, Cmd (Msg result), Maybe result )
bareModel model =
  ( model, Cmd.none, Nothing )

hasResults model =
  case model.searchResults of
    Nothing -> False
    Just _ -> True

hasSufficientText textInput = 
  String.length textInput > 2


-- VIEW

view
  : (result -> Html (Msg result))
  -> Model result
  -> Html (Msg result)

view singleResultView model =
  Html.form 
    [ onSubmit Submit, onWithOptions "click" swallow (Json.Decode.succeed ClickInside) ]
    [ input 
        [ placeholder "Enter user name" 
        , onInput TextInput
        , on "focus" (Json.Decode.map GotFocus Json.Decode.value)
        ] []
    , button [] [ text "search" ]
    , p [] [ text model.textInput ]
    , if model.isSearching then loadingIcon else text ""
    , p [] [ text <| "Expand: " ++ (toString model.shouldExpand) ]
    , p [] [ text <| "Is error: " ++ (toString model.isError) ]
    , resultsView singleResultView model.searchResults
    ]

resultsView singleResultView searchResults =
  case searchResults of
    Nothing -> text ""

    Just results ->
      let
        listItem = \result -> 
          li
            [ onWithOptions "click" swallow (Json.Decode.succeed (ResultSelected result)) ]
            [ singleResultView result ]

        listItems = List.map listItem results

        list = List.take 10 listItems
      in
        ul [] list

loadingIcon =
  img [ src "../images/Loading_icon.gif", alt "loading..." ] []

swallow = { stopPropagation = True, preventDefault = True }


-- SUBSCRIPTIONS

subscriptions : Model searchResult -> Sub (Msg searchResult)
subscriptions model =
  Mouse.clicks ClickOutside

-- PORTS

port selectText : Json.Encode.Value -> Cmd msg
