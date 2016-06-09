module App.Api.Api exposing (findUsers, SearchStatus(SearchFailed, SearchSucceed)) --where

import Http
import Task
import Json.Decode as Json

type SearchStatus result
  = SearchFailed Http.Error
  | SearchSucceed (List result)

findUsers : Json.Decoder (List result) -> String -> Cmd (SearchStatus result)
findUsers getUsersFromJson query =
  let
    url = getUrlFrom query
    get = Http.get getUsersFromJson url
  in
    Task.perform SearchFailed SearchSucceed get

getUrlFrom query =
  "https://api.github.com/search/users?q=" ++ query
