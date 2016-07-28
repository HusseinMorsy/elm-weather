module Config exposing (..)

-- set here you API-Key you got from http://www.openweathermap.org/appid#get


apiKey : String
apiKey =
    "8cf21be8619f02dc5c853e61b75aecaa"


type Unit
    = Celsius
    | Fahrenheit
    | Kelvin



-- set her the unit for the tempretures: Celsius, Fahrenheit or Kelvin


unitForTemreture : Unit
unitForTemreture =
    Celsius



-- don't change the following lines


unit : String
unit =
    case unitForTemreture of
        Celsius ->
            "metric"

        Fahrenheit ->
            "imperial"

        Kelvin ->
            ""
