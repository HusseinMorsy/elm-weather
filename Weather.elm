module Weather where

import String
import Signal
import Effects exposing (Effects)
import Html exposing (..)
import Html.Attributes exposing (style, value, type')
import Html.Events exposing (on, onClick, targetValue)
import Http
import Json.Decode as Json
import Task
import Debug

-- MODEL

type alias Model = 
  { cities: List City
  , name_input: String
  }

type alias City =
  { name : String
  , temp: Int
  }

init : (Model, Effects Action)
init =
  ( initialModel, Effects.none)

initialModel = 
  { cities = [ ]
  , name_input = ""
  }

-- UPDATE

type Action = 
  NoOp
  | UpdateNameField String
  | Add
  | NewTemp (Maybe Float)

update: Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    NoOp ->
     (model, Effects.none)
    UpdateNameField input ->
      ( { model | name_input <- input }, Effects.none )
    Add ->
      let
        newCity = City model.name_input 0
      in
        ( model , getTemp model.name_input )
    NewTemp temp ->
      let
        convertTemp = round(Maybe.withDefault 0.0 temp)
        newCity = City model.name_input convertTemp
      in
        ({ model | name_input <- "", cities <- newCity :: model.cities }, Effects.none )

-- VIEW


view: Signal.Address Action -> Model -> Html
view address model = 
 div 
   [ ] 
   [ h1 [] [ text "Cities" ]
   , cityForm address model
   , cities model
   ]

cityForm : Signal.Address Action -> Model -> Html
cityForm address model =
  form 
    [ ]
    [ label [ ] [ text "Ctiy: " ]
    , input [ onInput address UpdateNameField, value model.name_input] [ ]
    , input [ type' "button", value "Submit", onClick address Add] [ ]
    ]

onInput : Signal.Address a -> (String -> a) -> Attribute
onInput address f =
  on "input" targetValue (\v -> Signal.message address (f v))

cities : Model -> Html
cities model =
  let 
    input = 
      if String.length(model.name_input) > 0 then
        li [ ] [ text model.name_input ]
      else
        span [ ] [ ] 

  in
    ul [ ] ( input :: List.map city model.cities)

city: City -> Html
city city =
  let
    cityString = (city.name ++ " " ++ (toString city.temp) ++ "°C")
  in
    li [ ] [ text cityString ]

-- EFFECTS

getTemp : String -> Effects Action
getTemp cityName =
  -- Http.getString "http://api.openweathermap.org/data/2.5/weather?q=neuss&units=metric"
  Http.get decodeData (weatherURL cityName)
  |> Task.toMaybe
  |> Task.map NewTemp
  |> Effects.task

weatherURL: String -> String
weatherURL cityName =
  Http.url "http://api.openweathermap.org/data/2.5/weather" [ ("q", cityName), ("units", "metric")]

decodeData : Json.Decoder Float
decodeData = 
  Json.at [ "main", "temp"] Json.float
