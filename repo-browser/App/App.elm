port module App exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App as Html
import Html.Events as Events exposing (on, onInput)
import Time
import App.SearchUserBar.SearchUserBar as SearchUserBar exposing (SearchResult)
import App.UserRepos.UserReposList as UserReposList

import TimeTravel.Html.App as TimeTravel

type alias User = SearchResult

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
  , userReposList : UserReposList.Model
  , selectedUser : Maybe User
  }

init : ( Model, Cmd Msg )
init =
  let
    model =
      { searchUserBar = SearchUserBar.init
      , userReposList = UserReposList.init
      , selectedUser = Nothing
      }
  in
    ( model, Cmd.none )


-- UPDATE

type Msg
  = SearchBarMsg SearchUserBar.Msg
  | UserReposListMsg UserReposList.Msg

update : Msg -> Model -> ( Model, Cmd Msg )
update msg =
  case msg of
    SearchBarMsg msg -> handleSearchBarMsg msg
    UserReposListMsg msg -> handleUserReposListMsg msg


handleSearchBarMsg msg ({ searchUserBar, userReposList } as model) =
  let
    ( updatedSearchUserBar, searchUserBarCmd, selectedUser ) =
      SearchUserBar.update msg searchUserBar

    ( userReposList, userReposListCmd ) =
      case selectedUser of
        Just user ->
          UserReposList.update (UserReposList.Add user) model.userReposList

        Nothing -> ( model.userReposList, Cmd.none )

    newModel =
      { model |
        searchUserBar = updatedSearchUserBar
      , userReposList = userReposList
      , selectedUser = selectedUser
      }

    cmd =
      Cmd.batch
        [ Cmd.map SearchBarMsg searchUserBarCmd
        , Cmd.map UserReposListMsg userReposListCmd
        ]
  in
    ( newModel , cmd )

handleUserReposListMsg msg ({ userReposList } as model) =
  let ( userReposList, userReposListCmd ) =
    UserReposList.update msg userReposList
  in
    ( { model | userReposList = userReposList }
    , Cmd.map UserReposListMsg userReposListCmd
    )

-- VIEW

view : Model -> Html Msg
view ({ searchUserBar, userReposList } as model) =
  div []
    [ p [] [ text "Find user" ]
    , userReposView userReposList
    , searchUserBarView searchUserBar
    ]

searchUserBarView : SearchUserBar.Model -> Html Msg
searchUserBarView searchUserBar =
  Html.map SearchBarMsg (SearchUserBar.view searchUserBar)

userReposView : UserReposList.Model -> Html Msg
userReposView userReposList =
  Html.map UserReposListMsg (UserReposList.view userReposList)


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Sub.map SearchBarMsg (SearchUserBar.subscriptions model.searchUserBar)
    ]
