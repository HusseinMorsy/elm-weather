module Weather where

import String
import Signal
import Effects exposing (Effects)
import Html exposing (..)
import Html.Attributes exposing (style, value, type')
import Html.Events exposing (on, onClick, targetValue)

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
  {cities = [ City "Düsseldorf" 0,  City "München" 0 ]
  , name_input = ""
  }

-- UPDATE

type Action = 
  NoOp
  | UpdateNameField String
  | Add

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
        ( { model | name_input <- "", cities <- newCity :: model.cities }, Effects.none )

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

