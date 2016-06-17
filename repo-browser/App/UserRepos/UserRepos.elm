module App.UserRepos.UserRepos exposing (
  Model, 
  Msg(Inner, Outer),
  OuterMsg(NoOp, Remove),
  new,
  view,
  update
  ) --where

import Json.Decode as Json exposing (andThen, (:=))
import Date exposing (Date)
import Html exposing (..)
import Html.Attributes exposing (src, alt, href, width)
import Html.Events exposing (onClick)
import Html.App as App
import Debug

import App.Api.Api as Api
import App.Pagination.Pagination as Pagination

type alias Model =
  { userLogin : String
  , avatarUrl : String
  , userRepos : List Repo
  , areReposExpanded : Bool
  , isLoading : Bool
  , pagination : Maybe Pagination.Model
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
      , pagination = Nothing
      }

    getUserRepos =
      Cmd.map
        (\apiStatus -> ( Inner <| FetchStatus apiStatus, optimisticUser ) )
        (Api.getUserRepos getReposFromJson userLogin)
  in ( optimisticUser, getUserRepos )

reposPerPage = 10

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
  | PaginationMsg Pagination.Msg
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

    PaginationMsg msg ->
      let
        pagination = model.pagination `Maybe.andThen` 
          (Result.toMaybe << Pagination.update msg)
      in
        { model | pagination = pagination }       

    (FetchStatus status) ->
      case status of
        Api.FetchFailed error -> model --TODO

        Api.FetchSucceed repos ->
          let 
            reposCount = List.length repos
            pagination = 
              if reposCount > reposPerPage then
                Just <| Pagination.init reposPerPage reposCount
              else
                Nothing
          in
            { model | 
              userRepos = repos
            , isLoading = False
            , pagination = pagination 
            }


-- VIEW

view : Model -> Html Msg
view ({ userLogin, avatarUrl, userRepos, pagination } as model) =
  let
    paginationView = case pagination of
        Just pagination ->
          App.map 
            (\msg -> Inner (PaginationMsg msg)) 
            (Pagination.view pagination)

        Nothing -> text ""

  in          
    div [ onClick <| Inner Expand ]
      [ div [] [ text userLogin ]
      , img [ src avatarUrl, alt <| userLogin ++ "'s avatar", width 200 ] []
      , p [] [ text <| "repos: " ++ (List.length userRepos |> toString) ]
      , button [ onClick <| Outer Remove ] [ text "remove" ]
      , div [ onClick <| Inner Fold ] [ text "fold" ]
      , reposView model
      , paginationView
      ]

reposView : Model -> Html Msg
reposView { userRepos, pagination } =
  let
    repoView = \{ url, name, starCount } -> 
      li []
        [ a [ href url ] [ text name ]
        , span [] [ text <| " stars: " ++ (toString starCount) ]
        ]

    items = List.map repoView userRepos

    pagedItems = case pagination of
      Nothing -> items

      Just { itemsPerPage, currentPage } ->
        items 
          |> List.drop (currentPage * itemsPerPage)
          |> List.take itemsPerPage
  in
    ul [] pagedItems

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
-- handle github api limit for 30 items for page