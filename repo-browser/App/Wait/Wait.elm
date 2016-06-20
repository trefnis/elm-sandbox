module App.Wait.Wait exposing (Model, Msg, init, update, check) --where

import Time exposing (Time) 

--MODEL

type alias Model =
  { timeStartedWaiting : Time
  , expectedWaitingTime : Time
  , isReady : Bool
  }

type alias Progress = Float

init : Time -> Time -> Model
init expectedWaitingTime now =
  { timeStartedWaiting = now
  , expectedWaitingTime = expectedWaitingTime
  , isReady = False
  }


-- UPDATE

type Msg
  = Tick Time

update : Msg -> Model -> ( Model, Bool )
update 
  (Tick newTime) 
  ({ timeStartedWaiting
  , expectedWaitingTime
  } as model) =
  let
    timeAlreadyWaiting = Time.inMilliseconds (newTime - timeStartedWaiting)
    isReady = expectedWaitingTime <= timeAlreadyWaiting

    newModel = 
      { model | isReady = isReady }

    in ( newModel, isReady )

check : Model -> Sub Msg
check { isReady, expectedWaitingTime } =
  if isReady then
    Sub.none
  else
    Time.every expectedWaitingTime Tick
