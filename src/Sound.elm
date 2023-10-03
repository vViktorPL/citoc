port module Sound exposing (playSound, playMusic)

port playSound : String -> Cmd msg
port playMusic : String -> Cmd msg