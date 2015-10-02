import StartApp
import Weather exposing (init, view, update)
import Signal
import Task
import Effects

app = 
  StartApp.start
    { init = init
    , view = view
    , update = update
    , inputs = []
  }

main = 
  app.html

port tasks : Signal (Task.Task Effects.Never ())
port tasks =
    app.tasks
