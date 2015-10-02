module Weather where

import Signal
import Effects exposing (Effects)
import Html exposing (..)


-- MODEL

type alias Model = Int

init : (Model, Effects Action)
init = (3, Effects.none)

-- UPDATE

type Action = 
  NoOp

update: Action -> Model -> (Model, Effects Action)
update action model =
  (model, Effects.none)

-- VIEW


view: Signal.Address Action -> Model -> Html
view address model = 
 div [ ] [ h1 [] [ text "Hallo Welt" ] ]
