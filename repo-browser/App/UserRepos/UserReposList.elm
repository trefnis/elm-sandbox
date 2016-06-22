module App.UserRepos.UserReposList exposing
  ( Model
  , Msg(Add)
  , init
  , update
  , view
  )

import App.UserRepos.UserRepos as UserRepos
import App.SearchUserBar.SearchUserBar exposing (SearchResult)
import List
import Html exposing (..)
import Html.Events exposing (onClick)
import Html.App
import Debug

type alias User = SearchResult

-- MODEL

type alias Model =
  { userReposList : List UserRepos.Model }

init : Model
init =
  Model []


-- UPDATE

type Msg
  = Add User
  | UserReposMsg ( UserRepos.Msg, UserRepos.Model )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ userReposList } as model) =
  let ( newList, cmd ) =
    case msg of
      Add msg -> handleAdd msg model
      UserReposMsg msg -> handleUserReposMsg msg model
    in
      ( { model | userReposList = newList }, cmd )


handleAdd
  { userLogin, avatarUrl }
  { userReposList }
  =
  let ( newUserRepos, cmd ) = UserRepos.new userLogin avatarUrl
  in
    ( newUserRepos :: userReposList
    , Cmd.map UserReposMsg cmd
    )

handleUserReposMsg ( msg, updatedRepos ) { userReposList } =
  case msg of
    UserRepos.Inner innerMsg ->
      let
        update = (\repos ->
          if repos == updatedRepos then
            UserRepos.update innerMsg updatedRepos
          else
            ( repos, Cmd.none )
        )

        extractCmds cmds =
          let getUserRepoMsg msg = Cmd.map UserReposMsg msg
          in
            List.map getUserRepoMsg cmds |> Cmd.batch
      in
        List.map update userReposList
          |> List.unzip
          |> \( newUserRepos, cmds) -> ( newUserRepos, extractCmds cmds)

    UserRepos.Outer outerMsg ->
      let newUserRepos = case outerMsg of
        UserRepos.NoOp -> userReposList

        UserRepos.Remove ->
          List.filter (\repos -> updatedRepos /= repos) userReposList
      in
        ( newUserRepos, Cmd.none )


view : Model -> Html Msg
view model =
  let
    userReposView = \userRepos ->
      Html.App.map
        (\msg -> UserReposMsg ( msg, userRepos ))
        (UserRepos.view userRepos)

  in
    ul [] <|
      List.map
        (\repos ->
          li [] [ userReposView repos ]
        )
        model.userReposList
