port module WebBrowser exposing (ClipboardEvent(..), clipboardEvent, setClipboardCopyableText, windowShake)


port windowShakeInternal : (() -> msg) -> Sub msg


port setClipboardCopyableText : String -> Cmd msg


port clipboardEventInternal : (( String, Maybe String ) -> msg) -> Sub msg


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
