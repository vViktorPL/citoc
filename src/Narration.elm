module Narration exposing (Model, init, playNarration, update, view)

import Array exposing (Array)
import Html exposing (Html)
import Html.Attributes exposing (style)
import Sound


type Model
    = NoNarration
    | Narrating
        { subtitles : String
        , timeout : Float
        , remainingNarrations : List Int
        }


type alias NarrationEntry =
    { subtitles : String
    , length : Float
    , soundFile : String
    }


init =
    NoNarration


narrationTexts =
    Array.fromList
        [ { subtitles = "As children, we explore the world with boundless curiosity.", soundFile = "narration_1.mp3" }
        , { subtitles = "Our sandbox is a boundless ocean, a canvas for our limitless imagination.", soundFile = "narration_2.mp3" }
        , { subtitles = "We build imaginary kingdoms and dream beyond the horizon.", soundFile = "narration_3.mp3" }
        , { subtitles = "We're driven by a desire to understand, to conquer the unknown.", soundFile = "narration_4.mp3" }
        , { subtitles = "As we grow older, we begin to seek stability, an anchor in the midst of chaos.", soundFile = "narration_5.mp3" }
        , { subtitles = "Our perspective, shifts.", soundFile = "narration_6.mp3" }
        , { subtitles = "What was once an endless ocean is revealed to be a mere sandbox-bounded and finite.", soundFile = "narration_7.mp3" }
        , { subtitles = "Our vision sharpens, but so does the blade that cuts away our childlike wonder.", soundFile = "narration_8.mp3" }
        , { subtitles = "We build fences, we set rules—comforting illusions that lull us into a sense of permanence.", soundFile = "narration_9.mp3" }
        , { subtitles = "In the process, we trade the dynamism of youth for the security of patterns, losing the very creativity and open-mindedness that once propelled us.", soundFile = "narration_10.mp3" }
        , { subtitles = "Yet, every now and then, life forces us to confront the fragility of our constructs...", soundFile = "narration_11.mp3" }
        , { subtitles = "...shattering the glass through which we've chosen to see the world.", soundFile = "narration_12.mp3" }
        , { subtitles = "It is in these moments of clarity that we realize our convictions are but illusions, veils that have obscured our vision but never changed the essence of what lies beyond.", soundFile = "narration_13.mp3" }
        ]


playNarration : Model -> Int -> ( Model, Cmd msg )
playNarration model narrationNumber =
    case model of
        Narrating narration ->
            ( Narrating { narration | remainingNarrations = narration.remainingNarrations ++ [ narrationNumber ] }, Cmd.none )

        NoNarration ->
            playNarrationInternal model narrationNumber


playNarrationInternal : Model -> Int -> ( Model, Cmd msg )
playNarrationInternal model narrationNumber =
    case Array.get (narrationNumber - 1) narrationTexts of
        Just { subtitles, soundFile } ->
            ( Narrating
                { subtitles = subtitles
                , timeout =
                    subtitles
                        |> String.words
                        |> List.length
                        |> toFloat
                        |> (*) 500
                , remainingNarrations =
                    case model of
                        NoNarration ->
                            []

                        Narrating { remainingNarrations } ->
                            remainingNarrations
                }
            , Sound.playSound soundFile
            )

        Nothing ->
            ( model, Cmd.none )


update : Float -> Model -> ( Model, Cmd msg )
update delta model =
    case model of
        Narrating narration ->
            let
                newTime =
                    max 0 (narration.timeout - delta)
            in
            if newTime > 0 then
                ( Narrating { narration | timeout = newTime }, Cmd.none )

            else
                case narration.remainingNarrations of
                    nextNarration :: restNarrations ->
                        playNarrationInternal (Narrating { narration | remainingNarrations = restNarrations }) nextNarration

                    [] ->
                        ( NoNarration, Cmd.none )

        NoNarration ->
            ( model, Cmd.none )


view : Model -> Html msg
view model =
    case model of
        Narrating { subtitles } ->
            Html.div
                [ style "background" "black"
                , style "padding" "0.5em 1.5em"
                , style "color" "white"
                , style "position" "fixed"
                , style "bottom" "1.5em"
                , style "left" "50%"
                , style "transform" "translateX(-50%)"
                , style "max-width" "50vw"
                , style "font-size" "16px"
                ]
                [ Html.text subtitles ]

        NoNarration ->
            Html.text ""
