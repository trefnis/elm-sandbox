module App.Api.Api exposing (findUsers, getUserRepos, Status(FetchFailed, FetchSucceed)) --where

import Http
import Task exposing (Task)
import Json.Decode as Json

type Status result
  = FetchFailed Http.Error
  | FetchSucceed result

type alias Url = String

urlBase : Url
urlBase = "https://api.github.com/"

apiTask : Task Http.Error value -> Cmd (Status value)
apiTask task =
  Task.perform FetchFailed FetchSucceed task

apiGet : Json.Decoder result -> Url -> Cmd (Status result)
apiGet decoder url =
  apiTask <| Http.get decoder url 

findUsers : Json.Decoder result -> String -> Cmd (Status result)
findUsers getUsersFromJson query =
  let url = urlBase ++ "search/users?q=" ++ query
  in 
    apiGet getUsersFromJson url

getUser : Json.Decoder result -> String -> Cmd (Status result)
getUser getUserFromJson userName =
  let url = urlBase ++ "users/" ++ userName
  in
    apiGet getUserFromJson url

getUserRepos : Json.Decoder result -> String -> Cmd (Status result)
getUserRepos getUserReposFromJson userName =
  let
    url = urlBase ++ "users/" ++ userName ++ "/repos"
  in
    apiGet getUserReposFromJson url
