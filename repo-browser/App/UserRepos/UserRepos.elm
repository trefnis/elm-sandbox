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
  , reposCount : Int
  , areReposExpanded : Bool
  , isLoading : Bool
  , pagination : Maybe Pagination.Model
  }

init : String -> String -> Model
init userLogin avatarUrl =
  { userLogin = userLogin
  , avatarUrl = avatarUrl
  , userRepos = []
  , reposCount = 0
  , areReposExpanded = False
  , isLoading = True
  , pagination = Nothing
  }

new : String -> String -> ( Model, Cmd ( Msg, Model ) )
new userLogin avatarUrl = 
  let 
    optimisticUser = init userLogin avatarUrl
    cmd = getUserReposCmd optimisticUser 1
  in 
    ( optimisticUser, cmd )

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
  | FetchStatus (Api.Status ( List Repo, Int ))

type OuterMsg 
  = NoOp
  | Remove

update : InnerMsg -> Model -> ( Model, Cmd ( Msg, Model ) )
update msg model =
  case msg of
    Expand ->
      ( { model | areReposExpanded = True }, Cmd.none )

    Fold ->
      ( { model | areReposExpanded = False }, Cmd.none )

    PaginationMsg msg ->
      let
        pagination = model.pagination `Maybe.andThen` 
          (Result.toMaybe << Pagination.update msg)

        newModel = { model | pagination = pagination }
        page = 
          Maybe.withDefault 0
            <| Maybe.map .currentPage pagination

        cmd = 
          getUserReposCmd model (page + 1)
      in
        ( newModel, cmd )

    (FetchStatus status) ->
      let newModel = case status of
        Api.FetchFailed error -> model --TODO

        Api.FetchSucceed ( repos, reposCount ) ->
          let 
            pagination = 
              if reposCount > reposPerPage then
                Just <| Pagination.init reposPerPage reposCount
              else
                Nothing
          in
            Debug.log "fetch" <| { model | 
              userRepos = repos
            , isLoading = False
            , pagination = pagination 
            }
      in
        ( newModel, Cmd.none )

getUserReposCmd : Model -> Int -> Cmd ( Msg, Model )
getUserReposCmd model page =
  Cmd.map 
    (\msg -> ( msg, model )) 
    (getUserRepos model.userLogin page)

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


-- API

getUserRepos : String -> Int -> Cmd Msg
getUserRepos userLogin page =
  let
    userReposDecoder : Json.Decoder ( List Repo, Int )
    userReposDecoder = 
      ( Json.object2 (,)
          ("items" := getReposFromJson)
          ("total_count" := Json.int)
      )

    searchUserRepos = 
      Api.searchUserRepos 
        reposPerPage 
        userReposDecoder
        userLogin
        page
  in
    Cmd.map (Inner << FetchStatus) searchUserRepos

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
-- add throttling of autocomplete requests