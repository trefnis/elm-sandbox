module App.Wait.Wait exposing (Model, Msg, init, update, check, start) --where

import Time exposing (Time) 

--MODEL

type alias Model =
  Maybe
    { timeStartedWaiting : Time
    , expectedWaitingTime : Time
    , isReady : Bool
    }

type alias Progress = Float

init : Model
init = Nothing


-- UPDATE

type Msg
  = Tick Time

update : Msg -> Model -> ( Model, Bool )
update (Tick newTime) model =
  case model of
    Nothing -> ( Nothing, False )

    Just
      ({ timeStartedWaiting
      , expectedWaitingTime
      } as oldModel) ->
      let
        timeAlreadyWaiting = Time.inMilliseconds (newTime - timeStartedWaiting)
        isReady = expectedWaitingTime <= timeAlreadyWaiting

        newModel = if isReady then Nothing else Just oldModel

        in ( newModel, isReady )


start : Time -> Time -> Model
start expectedWaitingTime now =
  Just
    { timeStartedWaiting = now
    , expectedWaitingTime = expectedWaitingTime
    , isReady = False
    }

check : Model -> Sub Msg
check model =
  case model of
    Nothing -> Sub.none

    Just { expectedWaitingTime } ->
      Time.every expectedWaitingTime Tick
