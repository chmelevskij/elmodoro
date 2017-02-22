import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Time exposing (..)


main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL

type alias Model = 
  { minutes: Time
  , seconds: Time
  , paused : Bool
  }

-- INIT

init : (Model, Cmd Msg)
init =
  (Model 25 0 True, Cmd.none)

-- UPDATE

type Msg
  = Tick Time
    | Pause
    | SetTime String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick newTime ->
      if model.paused then
        ( model, Cmd.none )
      else
        (countdown model, Cmd.none)

    Pause ->
        ({ model | paused = not <| model.paused } , Cmd.none)

    SetTime newTime ->
      ( {model
      | minutes = strToMins newTime
      , seconds = 0
      , paused = True} , Cmd.none)


-- VIEW

view : Model -> Html Msg
view model =
  div[]
    [ h1 [][ text <| prettify <| model ]
    , button [ onClick Pause] [
      text
      <| if model.paused 
      then
        "Play"
      else
        "Pause"
        ]
          , showStatus model
          , input 
          [ onInput SetTime
          , disabled <| not <| model.paused
          , type_ "number"
          , min_ "0"
          , max_ "60" ][]
          ]



-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  if model.paused
  then Sub.none
  else every second Tick

-- HELPERS

strToMins: String -> Float
strToMins newTime =
  String.toFloat newTime
  |> Result.toMaybe 
  |> Maybe.withDefault 25

prettify : Model -> String
prettify model = 
  toString model.minutes
    ++ ":" 
    ++ toString model.seconds

countdown : Model -> Model
countdown model = 
  if model.minutes == 0 && model.seconds == 0
  then
    (Model 25 00 True)
  else if model.seconds == 0
  then
    { model |
      minutes = model.minutes - 1,
      seconds = 59
    }
  else
    { model |
      seconds = model.seconds - 1
    }


showStatus : Model -> Html msg
showStatus model =
  let
      (color, message) =
        if model.minutes == 25 && model.seconds == 00
        then
          ("green", "Break")
        else
          ("red", "Pomodoro")
  in
      div [ style [("color", color)]] [ text message ]


min_ val = Html.Attributes.min val
max_ val = Html.Attributes.max val
