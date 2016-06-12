module App.Api.Api exposing (findUsers, Status(FetchFailed, FetchSucceed)) --where

import Http
import Task
import Json.Decode as Json

type Status result
  = FetchFailed Http.Error
  | FetchSucceed result

findUsers : Json.Decoder result -> String -> Cmd (Status result)
findUsers getUsersFromJson query =
  let
    url = getUrlFrom query
    get = Http.get getUsersFromJson url
  in
    Task.perform FetchFailed FetchSucceed get

getUrlFrom query =
  "https://api.github.com/search/users?q=" ++ query
