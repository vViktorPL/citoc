module Ending exposing (Model, init, update, view)

import Html exposing (Html, div, text)
import Html.Attributes exposing (classList)
import Sound


type Model
    = EndingModel Float Bool


init : Model
init =
    EndingModel 0 False


turningBlackTime =
    3000


narrationStartTime =
    turningBlackTime + 3000


changeRevealTime =
    narrationStartTime + 950


isRevealTime =
    narrationStartTime + 2000


theRevealTime =
    narrationStartTime + 2250


onlyRevealTime =
    narrationStartTime + 2350


constantRevealTime =
    narrationStartTime + 2650


acronymRevealTime =
    constantRevealTime + 3000


acronymRevealFadeout =
    acronymRevealTime + 8000


update : Float -> Model -> ( Model, Cmd msg )
update delta (EndingModel timePassed narrationAlreadyStarted) =
    let
        newTimePassed =
            timePassed + delta

        shouldStartNarration =
            not narrationAlreadyStarted && timePassed >= narrationStartTime
    in
    ( EndingModel newTimePassed (shouldStartNarration || narrationAlreadyStarted)
    , if shouldStartNarration then
        Sound.playSound "narration_14.mp3"

      else
        Cmd.none
    )


view : Model -> Html msg
view (EndingModel timePassed _) =
    div [ classList [ ( "endingTextContainer", True ), ( "visible", timePassed > 0 ), ( "fadeOut", timePassed >= acronymRevealFadeout ) ] ]
        [ div [ classList [ ( "becauseText", True ), ( "visible", timePassed >= narrationStartTime && timePassed < acronymRevealTime ) ] ] [ text "Because..." ]
        , div [ classList [ ( "citoc", True ), ( "acronymOnly", timePassed >= acronymRevealTime ) ] ]
            [ div [ classList [ ( "visible", timePassed >= changeRevealTime ) ] ] [ text "Change" ]
            , div [ classList [ ( "visible", timePassed >= isRevealTime ) ] ] [ text "Is" ]
            , div [ classList [ ( "visible", timePassed >= theRevealTime ) ] ] [ text "The" ]
            , div [ classList [ ( "visible", timePassed >= onlyRevealTime ) ] ] [ text "Only" ]
            , div [ classList [ ( "visible", timePassed >= constantRevealTime ) ] ] [ text "Constant" ]
            ]
        ]
