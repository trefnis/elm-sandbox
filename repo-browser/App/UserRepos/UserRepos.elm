module App.UserRepos.UserRepos exposing (
  Model, 
  Msg(Inner, Outer),
  OuterMsg(NoOp, Remove),
  new,
  view,
  update
  ) --where

import App.Api.Api as Api
import Json.Decode as Json exposing (andThen, (:=))
import Date exposing (Date)
import Html exposing (..)
import Html.Attributes exposing (src, alt, href, width)
import Html.Events exposing (onClick)
import Debug

type alias Model =
  { userLogin : String
  , avatarUrl : String
  , userRepos : List Repo
  , areReposExpanded : Bool
  , isLoading : Bool
  , reposPerPage : Int
  , currentPage : Int
  }

new : String -> String -> ( Model, Cmd ( Msg, Model ) )
new userLogin avatarUrl = 
  let 
    optimisticUser =
      { userLogin = userLogin
      , avatarUrl = avatarUrl
      , userRepos = []
      , areReposExpanded = False
      , isLoading = True
      , reposPerPage = 5
      , currentPage = 0
      }

    getUserRepos =
      Cmd.map
        (\apiStatus -> ( Inner <| FetchStatus apiStatus, optimisticUser ) )
        (Api.getUserRepos getReposFromJson userLogin)
  in ( optimisticUser, getUserRepos )

type alias Repo =
  { url : String
  , name : String
  , starCount : Int
  , createdAt : Date
  , updatedAt : Date
  }

type ReposSortOrder
  = ReposSortOrder ReposSortProperty SortOrder

type ReposSortProperty
  = ByStarCount
  | ByName
  | ByCreateDate
  | ByUpdateDate

type SortOrder
  = Ascending
  | Descending

-- UPDATE

type Msg 
  = Inner InnerMsg
  | Outer OuterMsg

type InnerMsg
  = Expand
  | Fold
  | NextPage
  | PreviousPage
  | FetchStatus (Api.Status (List Repo))

type OuterMsg 
  = NoOp
  | Remove

update : InnerMsg -> Model -> Model
update msg model =
  case msg of
    Expand ->
      { model | areReposExpanded = True }

    Fold ->
      { model | areReposExpanded = False }

    NextPage ->
      let
        elementsCount = List.length model.userRepos
        maxPage = 
          ceiling 
            <| ((toFloat elementsCount) / (toFloat model.reposPerPage) - 1)
        nextPage = model.currentPage + 1
      in
        { model | 
          currentPage = 
            if nextPage <= maxPage then nextPage else model.currentPage
        }

    PreviousPage ->
      let
        previousPage = model.currentPage - 1
      in
        { model | 
          currentPage = 
            if previousPage >= 0 then previousPage else model.currentPage
        }

    (FetchStatus status) ->
      case status of
        Api.FetchFailed error -> model --TODO

        Api.FetchSucceed repos -> 
          { model | userRepos = repos, isLoading = False }


-- VIEW

view : Model -> Html Msg
view ({ userLogin, avatarUrl, userRepos } as model) =
  div [ onClick <| Inner Expand ]
    [ div [] [ text userLogin ]
    , img [ src avatarUrl, alt <| userLogin ++ "'s avatar", width 200 ] []
    , p [] [ text <| "repos: " ++ (List.length userRepos |> toString) ]
    , button [ onClick <| Outer Remove ] [ text "remove" ]
    , div [ onClick <| Inner Fold ] [ text "fold" ]
    , reposView model
    , p [ onClick <| Inner NextPage ] [ text "next" ]
    , p [ onClick <| Inner PreviousPage ] [ text "prev" ]
    ]

reposView : Model -> Html Msg
reposView { userRepos, reposPerPage, currentPage } =
  let 
    repoView = \{ url, name, starCount } -> 
      li []
        [ a [ href url ] [ text name ]
        , span [] [ text <| " stars: " ++ (toString starCount) ]
        ]
  in
    List.map repoView userRepos
      |> List.drop (currentPage * reposPerPage)
      |> List.take reposPerPage
      |> ul []
    --ul [] <| List.map repoView repos

getReposFromJson : Json.Decoder (List Repo)
getReposFromJson =
  Json.list <| 
    Json.object5 Repo
      ("url" := Json.string)
      ("name" := Json.string)
      ("stargazers_count" := Json.int)
      (("created_at" := Json.string) `andThen` parseDateDecoder)
      (("updated_at" := Json.string) `andThen` parseDateDecoder)

parseDateDecoder : String -> Json.Decoder Date
parseDateDecoder dateString =
  case Date.fromString dateString of
    Ok date -> Json.succeed date
    Err err -> Json.fail err

-- dodawanie userRepo na kliknięcie elementu z autocomplete (może być jakiś przycisk ew.) 
-- usuwanie userRepo na 'x' na elemencie listy userReposów
-- cache w localstorage listy userReposów
-- reorder userReposów, kolejność na podstawie różnych typów sortowania + wybrana przez usera (drag & drop do ustalania kolejności)
-- paginacja dla repo konkretnego usera