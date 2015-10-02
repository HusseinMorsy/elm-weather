module Weather where

import Signal
import Effects exposing (Effects)
import Html exposing (..)


-- MODEL

type alias Model = 
  { cities: List City
  }

type alias City =
  { name : String
  , temp: Int
  }

init : (Model, Effects Action)
init =
  ( {cities = [ City "Düsseldorf" 0,  City "München" 0 ]  }, Effects.none)

-- UPDATE

type Action = 
  NoOp

update: Action -> Model -> (Model, Effects Action)
update action model =
  (model, Effects.none)

-- VIEW


view: Signal.Address Action -> Model -> Html
view address model = 
 div 
   [ ] 
   [ h1 [] [ text "Cities" ]
   , cities model
   ]

cities : Model -> Html
cities model =
  ul [ ] (List.map city model.cities)

city: City -> Html
city city =
  li [ ] [ text (city.name ++ " " ++ (toString city.temp) ++ "°C") ]
