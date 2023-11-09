port module Sound exposing (playMusic, playSound, stopMusic)


port playSound : String -> Cmd msg


port playMusic : String -> Cmd msg


port stopMusic : () -> Cmd msg
