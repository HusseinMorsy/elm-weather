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

convertTemp : Maybe Float -> Int
convertTemp temp= round(Maybe.withDefault (toFloat unknownTemp) temp)

unknownTemp = 0
-- UPDATE

type Action =
  NoOp
  | UpdateNameField String
  | Add
  | NewTemp (Maybe Float)
  | Delete Id
  | RequestUpdate City
  | RequestUpdateAll
  | UpdateTemp Id (Maybe Float)

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
        newCity = City model.nextId model.nameInput (convertTemp temp)
      in
        ({ model | nameInput <- "", cities <- newCity :: model.cities, nextId <- model.nextId + 1 }, Effects.none )
    Delete id ->
      let
        citiesDeleted = List.filter (\e -> e.id /= id) model.cities
      in
      ( { model | cities <- citiesDeleted }, Effects.none )
    RequestUpdate city ->
      ( model, getUpdatedTemp city )
    UpdateTemp id temp ->
      let
        changeCity = \e -> {e | temp <- if e.id == id then (convertTemp temp) else e.temp }
        updatedCity = List.map changeCity model.cities
      in
        ( { model | cities <- updatedCity}, Effects.none )
    RequestUpdateAll ->
      let
        updateCities = List.map (\c -> getUpdatedTemp c) model.cities
      in
        ( model, Effects.batch updateCities)

-- VIEW


view: Signal.Address Action -> Model -> Html
view address model =
 div
   [ ]
   [ h1 [] [ text "Cities" ]
   , cityForm address model
   , cities address model
   ]

cityForm : Signal.Address Action -> Model -> Html
cityForm address model =
  form
    [ ]
    [ label [ ] [ text "Ctiy: " ]
    , input [ onInput address UpdateNameField, value model.nameInput] [ ]
    , input [ type' "button", value "Submit", onClick address Add] [ ]
    , input [ type' "button", value "Update All", onClick address RequestUpdateAll ] [ ]
    ]

onInput : Signal.Address a -> (String -> a) -> Attribute
onInput address f =
  on "input" targetValue (\v -> Signal.message address (f v))

cities : Signal.Address Action ->  Model -> Html
cities address model =
    table [ ] ( List.map (city address) model.cities)

city: Signal.Address Action -> City -> Html
city address city =
  let
    cityTemp = if city.temp /= unknownTemp then ((toString city.temp) ++ "°C") else "?"
  in
    tr [ ]
      [ td [ ] [ text (toString city.id) ]
      , td [ ] [ text city.name ]
      , td [ ] [ text cityTemp ]
      , td [ ] [ button [ onClick address (RequestUpdate city)] [ text "Update" ] ]
      , td [ ] [ button [ onClick address (Delete city.id)] [ text "delete" ] ]
      ]

-- EFFECTS

getTemp : String -> Effects Action
getTemp cityName =
  -- Http.getString "http://api.openweathermap.org/data/2.5/weather?q=neuss&units=metric"
  Http.get decodeData (weatherURL cityName)
  |> Task.toMaybe
  |> Task.map NewTemp
  |> Effects.task

getUpdatedTemp : City -> Effects Action
getUpdatedTemp city =
  -- Http.getString "http://api.openweathermap.org/data/2.5/weather?q=neuss&units=metric"
  Http.get decodeData (weatherURL city.name)
  |> Task.toMaybe
  |> Task.map (UpdateTemp city.id)
  |> Effects.task

weatherURL: String -> String
weatherURL cityName =
  Http.url "http://api.openweathermap.org/data/2.5/weather" [ ("q", cityName), ("units", "metric")]

decodeData : Json.Decoder Float
decodeData =
  Json.at [ "main", "temp"] Json.float
