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
  , selectedLogin : String
  }

init : ( Model, Cmd Msg )
init =
  let 
    model = 
      { searchUserBar = SearchUserBar.init
      , selectedLogin = ""
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

        login =
          case selectedLogin of
            Just userLogin -> userLogin
            Nothing -> model.selectedLogin

        newModel = 
          { model | searchUserBar = updatedSearchUserBar, selectedLogin = login }

        cmd =
          Cmd.map SearchBarMsg searchUserBarCmd

      in
        ( newModel , cmd )


-- VIEW

view : Model -> Html Msg
view ({ searchUserBar } as model) =
  div []
    [ p [] [ text "Find user" ]
    , p [] [ text <| "user: " ++ model.selectedLogin ]
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
