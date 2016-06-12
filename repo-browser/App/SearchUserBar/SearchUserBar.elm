module App.SearchUserBar.SearchUserBar exposing (Model, Msg, init, update, view, subscriptions) --where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick, onSubmit, onFocus, onBlur)
import String
import Mouse
import Json.Decode as Json exposing ((:=))

import App.Api.Api as Api
import App.SearchBar.SearchBar as SearchBar

-- MODEL

type alias Model = SearchBar.Model SearchResult

init = SearchBar.init

type alias SearchResult = 
  { userLogin : String
  , avatarUrl : String 
  }


-- UPDATE

type alias Msg = SearchBar.Msg SearchResult

update : Msg -> Model -> ( Model, Cmd Msg, Maybe String )
update msg model =
  let
    (model, cmd, selectedResult) =
      SearchBar.update findUsers msg model

    selectedLogin = Maybe.map .userLogin selectedResult
  in
    (model, cmd, selectedLogin)


-- VIEW

view : Model -> Html Msg
view model =
  SearchBar.view singleResultView model

singleResultView result =
  div []
    [ p [] [ text result.userLogin ]
    , img
        [ src result.avatarUrl
        , width 50
        ] []
    ]

subscriptions model =
  SearchBar.subscriptions model

-- API

findUsers : String -> Cmd Msg
findUsers query =
  let find = Api.findUsers getUsersFromJson query
  in Cmd.map SearchBar.SearchStatus find

getUsersFromJson : Json.Decoder (List SearchResult)
getUsersFromJson =
  let
    userDecoder = Json.object2 SearchResult
      ("login" := Json.string)
      ("avatar_url" := Json.string)
  in
    Json.at ["items"] (Json.list userDecoder)
