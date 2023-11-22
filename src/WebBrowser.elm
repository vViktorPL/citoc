port module WebBrowser exposing (windowShake)


port windowShakeInternal : (() -> msg) -> Sub msg


windowShake : msg -> Sub msg
windowShake msg =
    windowShakeInternal (always msg)
