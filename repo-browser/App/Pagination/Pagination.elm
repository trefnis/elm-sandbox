module App.Pagination.Pagination exposing (Model, Msg, init, update, view)

import Html exposing (Html, div, ul, li, button, text)
import Html.Events exposing (onClick)
import Html.Attributes exposing (disabled)
import Debug

-- MODEL

type alias Model =
  { currentPage : Int
  , itemsPerPage : Int
  , elementsCount : Int
  }

init : Int -> Int -> Model
--init itemsPerPage elementsCount =
init = Model 0

-- UPDATE

type Msg = Page Int

outOfRange = Err "out of range"

update : Msg -> Model -> Result String Model
update (Page number) ({ currentPage } as model) =
  let newCurrentPageResult =
    if number >= 0 && number <= maxPage model then
      Ok number
    else
      outOfRange
  in
  case newCurrentPageResult of
    Err err -> Err err
    Ok newCurrentPage ->
      Ok { model | currentPage = newCurrentPage }

maxPage : Model -> Int
maxPage { elementsCount, itemsPerPage } =
  Debug.log "" (ceiling <| (toFloat elementsCount) / (toFloat itemsPerPage) - 1)

canMoveNext : Model -> Bool
canMoveNext model =
    model.currentPage + 1 <= maxPage model

canMoveBack : Model -> Bool
canMoveBack model =
  model.currentPage > 0


-- VIEW

view : Model -> Html Msg
view ({ currentPage } as model) =
  let
    lastPage = maxPage model
    from = max 0 (currentPage - 1)
    to = min lastPage (currentPage + 1)

    middleNumbers = numbersBetween from to

    middleNumbersView = List.map (flip regularPageView <| currentPage) middleNumbers

    pageNumbersView =
      [ prevView currentPage
      , firstNumberView middleNumbers currentPage
      ]
      ++
        middleNumbersView
      ++
      [ lastNumberView middleNumbers lastPage currentPage
      , nextView currentPage lastPage
      ]
  in
    ul [] pageNumbersView

pageNumberView number pageText currentPage =
  li [ onClick (Page number) ]
    [ text
        <| pageText ++ if number == currentPage then "*" else ""
    ]

regularPageView number = pageNumberView number (toString <| number + 1)

firstNumberView middleNumbers =
  if List.member 0 middleNumbers then
    always <| text ""
  else if List.member 1 middleNumbers then
    regularPageView 0
  else
    pageNumberView 0 "1 ..."

lastNumberView middleNumbers lastPage =
  if List.member lastPage middleNumbers then
    always <| text ""
  else if List.member (lastPage - 1) middleNumbers then
    regularPageView lastPage
  else
    pageNumberView lastPage ("... " ++ toString lastPage)

prevView current =
  li []
    [ button
        [ onClick (Page <| current - 1)
        , disabled (current <= 0)
        ]
        [ text "prev " ]
    ]

nextView current last =
  li []
    [ button
        [ onClick (Page <| current + 1)
        , disabled (current >= last)
        ]
        [ text "next" ]
    ]


numbersBetween : Int -> Int -> List Int
numbersBetween from to =
  if from == to then
    [from]
  else
    from :: numbersBetween (from + 1) to
