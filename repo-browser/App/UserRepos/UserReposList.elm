module App.UserRepos.UserReposList exposing (Model, Msg(Add), init, update, view)--where

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
  { userReposList : List UserRepos.Model
  , sorting : ( ReposSortProperty, SortOrder )
  }

init : Model
init =
  Model [] ( ByUserOrder, Ascending )


-- UPDATE

type Msg
  = Add User
  | UserReposMsg ( UserRepos.Msg, UserRepos.Model )
  | Sort ( ReposSortProperty, SortOrder )

type ReposSortProperty
  = ByReposStarSum
  | ByMostPopularRepoStarCount
  | ByReposCount
  | ByName
  | ByCreateDate
  | ByUserOrder

type SortOrder
  = Ascending
  | Descending

update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ userReposList, sorting } as model) =
  let ( newList, cmd ) =
    case msg of
      Add { userLogin, avatarUrl } ->
        let ( newUserRepos, cmd ) = UserRepos.new userLogin avatarUrl
        in
          ( sortBy sorting (newUserRepos :: userReposList)
          , Cmd.map UserReposMsg cmd
          )

      UserReposMsg ( msg, updatedRepos ) ->
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
              List.map update model.userReposList
                |> List.unzip
                |> \( newUserRepos, cmds) -> ( newUserRepos, extractCmds cmds)

          UserRepos.Outer outerMsg ->
            let newUserRepos = case outerMsg of
              UserRepos.NoOp -> userReposList

              UserRepos.Remove ->
                List.filter (\repos -> updatedRepos /= repos) userReposList
            in
              ( newUserRepos, Cmd.none )

      Sort sorting ->
        ( sortBy sorting userReposList, Cmd.none )
    in
      ( { model | userReposList = newList }, cmd )


sortBy 
  : ( ReposSortProperty, SortOrder ) 
  -> List UserRepos.Model 
  -> List UserRepos.Model

sortBy ( prop, order) list =
  list


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
