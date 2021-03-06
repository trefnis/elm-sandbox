port module App.SearchBar.SearchBar exposing
  ( Model
  , Msg(SearchStatus, ResultSelected)
  , init
  , update
  , view
  , subscriptions
  )

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
  , waitModel : Wait.Model
  }

init : Model searchResult
init =
  { textInput = ""
  , searchResults = Nothing
  , isSearching = False
  , isError = False
  , shouldExpand = False
  , waitModel = Wait.init
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

update find msg =
  case msg of
    GotFocus event -> handleGotFocus event
    ClickInside -> handleClickInside
    ClickOutside _ -> handleClickOutside find
    LostFocus -> handleLostFocus
    TextInput input -> handleTextInput input
    WaitMsg msg -> handleWaitMsg find msg
    StartWaitingForInput waitModel -> handleStartWaitingForInput waitModel
    Submit -> handleSubmit find
    SearchStatus status -> handleSearchStatus status
    ResultSelected result -> handleResultSelected result

handleGotFocus event model =
  ( { model | shouldExpand = hasResults model }
  , selectText event
  , Nothing
  )

handleClickInside model =
  bareModel model

handleClickOutside find model =
  update find LostFocus model

handleLostFocus model =
  bareModel { model | shouldExpand = False }

handleTextInput input model =
  let
    shouldSubmit = hasSufficientText input

    model =
      { model |
        textInput = input
      , shouldExpand = False
      , searchResults = Nothing
      , waitModel = Wait.init
      }

    waitDuration = (Time.millisecond * 500)

    startWaitingCmd =
      Task.perform
        (always Submit)
        (\now -> StartWaitingForInput <| Wait.start waitDuration now)
        Time.now
  in
    ( model
    , if hasSufficientText input then startWaitingCmd else Cmd.none
    , Nothing
    )

handleWaitMsg find msg model =
  let
    ( newWaitModel, isReady ) = Wait.update msg model.waitModel
    newModel = { model | waitModel = newWaitModel }
  in
    if isReady then
      update find Submit newModel
    else
      bareModel newModel

handleStartWaitingForInput waitModel model =
  bareModel { model | waitModel = waitModel }

handleSubmit find model =
  let newModel = { model | shouldExpand = True, isSearching = True }
  in
    ( newModel
    , find model.textInput
    , Nothing
    )

handleSearchStatus status model =
  case status of
    Api.FetchFailed error ->
      bareModel { model | isError = True, isSearching = False }

    Api.FetchSucceed results ->
      let
        searchResults = Just results

        newModel =
          { model |
            isSearching = False
          , searchResults = searchResults
          }
      in
        bareModel newModel

handleResultSelected result model =
  ( { model | shouldExpand = False }
  , Cmd.none
  , Just result
  )

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
  Sub.batch
    [ Mouse.clicks ClickOutside
    , Sub.map WaitMsg <| Wait.check model.waitModel
    ]

-- PORTS

port selectText : Json.Encode.Value -> Cmd msg
