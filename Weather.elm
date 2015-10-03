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
  , nameInput: String
  , nextId : Id
  }

type alias City =
  { id : Id
  , name : String
  , temp: Int
  }

type alias Id = Int

init : (Model, Effects Action)
init =
  ( initialModel, Effects.none)

initialModel = 
  { cities = [ ]
  , nameInput = ""
  , nextId = 0
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
      ( { model | nameInput <- input }, Effects.none )
    Add ->
      ( model , getTemp model.nameInput )
    NewTemp temp ->
      let
        convertTemp = round(Maybe.withDefault 0.0 temp)
        newCity = City model.nextId model.nameInput convertTemp
      in
        ({ model | nameInput <- "", cities <- newCity :: model.cities, nextId <- model.nextId + 1 }, Effects.none )

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
    , input [ onInput address UpdateNameField, value model.nameInput] [ ]
    , input [ type' "button", value "Submit", onClick address Add] [ ]
    ]

onInput : Signal.Address a -> (String -> a) -> Attribute
onInput address f =
  on "input" targetValue (\v -> Signal.message address (f v))

cities : Model -> Html
cities model =
  let 
    input = 
      if String.length(model.nameInput) > 0 then
        li [ ] [ text model.nameInput ]
      else
        span [ ] [ ] 

  in
    table [ ] ( input :: List.map city model.cities)

city: City -> Html
city city =
  let
    cityTemp = ((toString city.temp) ++ "°C")
  in
    tr [ ] 
      [ td [ ] [ text (toString city.id) ]
      , td [ ] [ text city.name ]
      , td [ ] [ text cityTemp ]
      ]

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
