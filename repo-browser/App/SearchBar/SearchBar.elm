port module App.SearchBar.SearchBar exposing (Model, Msg(SearchStatus, ResultSelected), init, update, view, subscriptions) --where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick, onSubmit, onFocus, onBlur, onWithOptions, on)
import String
import Mouse
import Time
import Task
import Json.Decode exposing ((:=))
import Json.Encode

import App.Api.Api as Api
import App.Wait.Wait as Wait

-- MODEL

type alias Model searchResult =
  { textInput : String
  , searchResults : Maybe (List searchResult)
  , isSearching : Bool
  , isError : Bool
  , shouldExpand : Bool
  , maybeWaitModel : Maybe Wait.Model
  }

init : Model searchResult
init =
  { textInput = ""
  , searchResults = Nothing
  , isSearching = False
  , isError = False
  , shouldExpand = False
  , maybeWaitModel = Nothing 
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
  | WaitMsg Wait.Msg
  | StartWaitingForInput Wait.Model

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
          , maybeWaitModel = Nothing
          }
        waitDuration = (Time.millisecond * 500)
        startWaitingCmd =
          Task.perform 
            (always Submit)
            (\now -> StartWaitingForInput <| Wait.init waitDuration now)
            Time.now
      in
        ( model
        , if hasSufficientText input then startWaitingCmd else Cmd.none
        , Nothing 
        )

    WaitMsg msg ->
      case model.maybeWaitModel of
        Nothing -> bareModel model

        Just waitModel ->
          let 
            ( newWaitModel, isReady ) = Wait.update msg waitModel
            newMaybeWaitModel = if not isReady then Just newWaitModel else Nothing

            newModel = { model | maybeWaitModel = newMaybeWaitModel }
          in 
            if isReady then
              update find Submit newModel
            else
              bareModel newModel

    StartWaitingForInput waitModel ->
      bareModel { model | maybeWaitModel = Just waitModel }

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
  let waitSub = 
    case model.maybeWaitModel of
      Just waitModel ->
        Sub.map WaitMsg <| Wait.check waitModel

      Nothing -> Sub.none
  in
    Sub.batch
      [ Mouse.clicks ClickOutside
      , waitSub
      ]

-- PORTS

port selectText : Json.Encode.Value -> Cmd msg
