
module Weather where

import Config
import Signal
import Effects exposing (Effects)
import Html exposing (..)
import Html.Attributes exposing (style, value, type', src)
import Html.Events exposing (on, onClick, targetValue)
import Http
import Json.Decode as Json
import Task


-- MODEL

type alias Model =
    { cities: List City
    , nameInput: String
    , nextId : Id
    }


type alias City =
    { id : Id
    , name : String
    , temp: Maybe.Maybe Int
    , loadingState: LoadingState
    }


type alias Id = Int


type LoadingState
    = Progress
    | Completed


init : (Model, Effects Action)
init =
  ( initialModel, Effects.none)

initialModel : Model
initialModel =
  { cities = [ ]
  , nameInput = ""
  , nextId = 0
  }


-- UPDATE

type Action
    = UpdateNameField String
    | AddCity
    | DeleteCity Id
    | RequestTempUpdate City
    | RequestTempUpdateAll
    | UpdateTemp Id (Maybe Float)
    | SortByCity


update: Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    UpdateNameField input ->
      ( { model | nameInput <- input }, Effects.none )

    AddCity ->
      let
      newCity = City model.nextId model.nameInput Nothing Progress
      in
        ({ model |
             nameInput <- "",
             cities <- newCity :: model.cities,
             nextId <- model.nextId + 1 },
           getUpdatedTemp newCity )

    DeleteCity id ->
      let
        citiesDeleted = List.filter (\e -> e.id /= id) model.cities
      in
      ( { model | cities <- citiesDeleted }, Effects.none )

    RequestTempUpdate city ->
      let
        changeCity = \e -> {e | loadingState  <- if e.id == city.id then Progress else e.loadingState }
        updateCities = List.map changeCity model.cities
      in
        ( { model | cities <- updateCities }, getUpdatedTemp city )

    UpdateTemp id temp ->
      let
        convertTemp temp= Maybe.map round temp
        changeCity = \e -> {e | loadingState <- Completed, temp <- if e.id == id then (convertTemp temp) else e.temp }
        updatedCity = List.map changeCity model.cities
      in
        ( { model | cities <- updatedCity}, Effects.none )
    RequestTempUpdateAll ->
      let
        updateCities = List.map (\c -> getUpdatedTemp c) model.cities
        setProgress = List.map (\c -> { c | loadingState <- Progress }) model.cities
      in
        ( { model | cities <- setProgress}, Effects.batch updateCities)
    SortByCity ->
      let
        sortCities = List.sortBy .name model.cities
      in
        ( { model | cities <- sortCities }, Effects.none)


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
  div
    [ ]
    [ label [ ] [ text "Ctiy: " ]
    , input [ onInput address UpdateNameField, value model.nameInput] [ ]
    , input [ type' "button", value "Add city", onClick address AddCity] [ ]
    , input [ type' "button", value "Update all", onClick address RequestTempUpdateAll ] [ ]
    , input [ type' "button", value "Sort by city", onClick address SortByCity ] [ ]
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
    tempToString = Maybe.withDefault "..." (Maybe.map toString city.temp)
    cityTemp = case city.loadingState of
      Progress ->
        spinner
      Completed ->
         text (tempToString ++ "°C")
  in
    tr [ ]
      [ td [  style [("min-width", "2em")] ] [ text (toString city.id) ]
      , td [  style [("min-width", "12em")] ] [ text city.name ]
      , td [ style [("width", "6em")] ] [ cityTemp ]
      , td [ ] [ button [ onClick address (RequestTempUpdate city)] [ text "Update" ] ]
      , td [ ] [ button [ onClick address (DeleteCity city.id)] [ text "delete" ] ]
      ]

spinner : Html
spinner =
  img [src "assets/spinner.gif"] []



-- EFFECTS

getUpdatedTemp : City -> Effects Action
getUpdatedTemp city =
  Http.get decodeData (weatherURL city.name)
  |> Task.toMaybe
  |> Task.map (UpdateTemp city.id)
  |> Effects.task

weatherURL: String -> String
weatherURL cityName =
  Http.url "http://api.openweathermap.org/data/2.5/weather" [ ("q", cityName), ("units", Config.unit), ("APPID", Config.apiKey)]

decodeData : Json.Decoder Float
decodeData =
  Json.at [ "main", "temp"] Json.float
