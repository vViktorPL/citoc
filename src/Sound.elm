port module Sound exposing (playSound, playMusic, stopMusic)

port playSound : String -> Cmd msg
port playMusic : String -> Cmd msg
port stopMusic : () -> Cmd msg
