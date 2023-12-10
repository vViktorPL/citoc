port module WebBrowser exposing (ClipboardEvent(..), clipboardEvent, saveState, setClipboardCopyableText, windowShake)

import Json.Encode as E


port windowShakeInternal : (() -> msg) -> Sub msg


port setClipboardCopyableText : String -> Cmd msg


port clipboardEventInternal : (( String, Maybe String ) -> msg) -> Sub msg


port saveState : ( String, E.Value ) -> Cmd msg


type ClipboardEvent
    = ClipboardCopy
    | ClipboardCut
    | ClipboardPaste String


windowShake : msg -> Sub msg
windowShake msg =
    windowShakeInternal (always msg)


clipboardEvent : (ClipboardEvent -> msg) -> Sub msg
clipboardEvent msg =
    clipboardEventInternal
        (\e ->
            msg
                (case e of
                    ( "cut", _ ) ->
                        ClipboardCut

                    ( "paste", Just content ) ->
                        ClipboardPaste content

                    _ ->
                        ClipboardCopy
                )
        )
