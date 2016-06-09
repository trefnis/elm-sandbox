port module App exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App as Html
import Html.Events as Events exposing (on, onInput)
import App.SearchUserBar.SearchUserBar as SearchUserBar

import TimeTravel.Html.App as TimeTravel

main =
  Html.program
  --TimeTravel.program
    { view = view
    , update = update
    , init = init
    , subscriptions = subscriptions
    }


-- MODEL

type alias Model = 
  { searchUserBar : SearchUserBar.Model
  }

init : ( Model, Cmd Msg )
init =
  let 
    model = 
      { searchUserBar = SearchUserBar.init
      }
  in
    ( model, Cmd.none )


-- UPDATE

type Msg
  = NoOp
  | SearchBarMsg SearchUserBar.Msg

update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ searchUserBar } as model) =
  case msg of
    NoOp ->
      ( model, Cmd.none )

    SearchBarMsg msg ->
      let 
        ( updatedSearchUserBar, searchUserBarCmd, selectedLogin ) = 
          SearchUserBar.update msg searchUserBar

        someText =
          case selectedLogin of
            Just userLogin -> userLogin
            Nothing -> "nothing"

        newModel = 
          { model | searchUserBar = updatedSearchUserBar }

        cmd =
          Cmd.map SearchBarMsg searchUserBarCmd

      in
        ( newModel , cmd )


-- VIEW

view : Model -> Html Msg
view ({ searchUserBar } as model) =
  div []
    [ p [] [ text "Find user" ]
    , searchUserBarView searchUserBar
    ]

searchUserBarView : SearchUserBar.Model -> Html Msg
searchUserBarView searchUserBar =
  Html.map SearchBarMsg (SearchUserBar.view searchUserBar)


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Sub.map SearchBarMsg (SearchUserBar.subscriptions model.searchUserBar)
    ]
