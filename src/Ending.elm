module Ending exposing (Model, init, update, view)

import Acceleration
import Angle
import Camera3d
import Color
import Direction3d
import Duration
import Frame3d
import Html exposing (Html, div, text)
import Html.Attributes exposing (classList)
import Length
import Obj.Decode exposing (ObjCoordinates)
import Physics.Body
import Physics.World
import Pixels
import Point3d
import Scene3d
import SceneAssets
import SketchPlane3d
import Sound
import Vector3d
import Viewpoint3d


type alias SceneWorld =
    Physics.World.World (Scene3d.Entity ObjCoordinates)


type Model
    = TitleReveal Float Bool SceneWorld
    | Credits Float SceneWorld


origin =
    Frame3d.atOrigin


init : SceneAssets.Model -> Model
init sceneAssets =
    let
        tangram =
            SceneAssets.tangram sceneAssets
                |> List.map (Physics.Body.translateBy (Vector3d.unsafe { x = 0, y = 0, z = 0.3 }))

        world =
            List.foldl Physics.World.add Physics.World.empty tangram
                |> Physics.World.add (Physics.Body.plane Scene3d.nothing)
                |> Physics.World.withGravity (Acceleration.metersPerSecondSquared (9.80665 * 0.1)) Direction3d.negativeZ
    in
    TitleReveal 0 False world


narrationStartTime =
    1000


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


thxSongStartTime =
    acronymRevealFadeout + 2000


authorCreditsTime =
    thxSongStartTime + 2300


authorCreditsFadeout =
    thxSongStartTime + 6300


specialThanksTime =
    thxSongStartTime + 7500


specialThanksFadeout =
    thxSongStartTime + 9000


evanTime =
    thxSongStartTime + 10800


evanFadeout =
    thxSongStartTime + 15000


ianTime =
    thxSongStartTime + 17000


ianFadeout =
    thxSongStartTime + 24000


sceneSpinningStartTime =
    thxSongStartTime + 21500


unsoundscapesTime =
    thxSongStartTime + 27000


physicsStartTime =
    unsoundscapesTime + 2000


unsoundscapesFadeout =
    thxSongStartTime + 32000


lueTime =
    thxSongStartTime + 34000


lueFadeout =
    thxSongStartTime + 39000


update : Float -> Model -> ( Model, Cmd msg )
update delta model =
    case model of
        TitleReveal timePassed narrationAlreadyStarted world ->
            let
                newTimePassed =
                    timePassed + delta

                shouldStartNarration =
                    not narrationAlreadyStarted && timePassed >= narrationStartTime
            in
            if timePassed >= thxSongStartTime then
                ( Credits timePassed world, Sound.playMusic "thx-song.mp3" )

            else
                ( TitleReveal newTimePassed (shouldStartNarration || narrationAlreadyStarted) world
                , if shouldStartNarration then
                    Sound.playSound "narration_14.mp3"

                  else
                    Cmd.none
                )

        Credits timePassed world ->
            let
                newTimePassed =
                    timePassed + delta

                newWorld =
                    if timePassed >= physicsStartTime && timePassed <= lueTime then
                        world
                            |> Physics.World.simulate (Duration.milliseconds delta)

                    else
                        world
            in
            ( Credits newTimePassed newWorld, Cmd.none )


view : Model -> ( Int, Int ) -> Html msg
view model canvasSize =
    case model of
        TitleReveal timePassed _ _ ->
            div [ classList [ ( "endingTextContainer", True ), ( "fadeOut", timePassed >= acronymRevealFadeout ) ] ]
                [ div [ classList [ ( "becauseText", True ), ( "visible", timePassed >= narrationStartTime && timePassed < acronymRevealTime ) ] ] [ text "Because..." ]
                , div [ classList [ ( "citoc", True ), ( "acronymOnly", timePassed >= acronymRevealTime ) ] ]
                    [ div [ classList [ ( "visible", timePassed >= changeRevealTime ) ] ] [ text "Change" ]
                    , div [ classList [ ( "visible", timePassed >= isRevealTime ) ] ] [ text "Is" ]
                    , div [ classList [ ( "visible", timePassed >= theRevealTime ) ] ] [ text "The" ]
                    , div [ classList [ ( "visible", timePassed >= onlyRevealTime ) ] ] [ text "Only" ]
                    , div [ classList [ ( "visible", timePassed >= constantRevealTime ) ] ] [ text "Constant" ]
                    ]
                ]

        Credits timePassed world ->
            div [ classList [ ( "credits", True ) ] ]
                [ viewCreditText ( authorCreditsTime, authorCreditsFadeout ) [ "Programming & concept:", "Wiktor Toporek" ] timePassed
                , viewCreditText ( specialThanksTime, specialThanksFadeout ) [ "Special thanks to:" ] timePassed
                , viewCreditText ( evanTime, evanFadeout ) [ "Evan Czaplicki", "for creating", "delightful Elm language" ] timePassed
                , viewCreditText ( ianTime, ianFadeout ) [ "Ian Mackenzie", "for creating", "elm-3d-scene and many other wonderful Elm packages" ] timePassed
                , viewCreditText ( unsoundscapesTime, unsoundscapesFadeout ) [ "Andrey Kuzmin", "for creating", "elm-physics and elm-obj-file" ] timePassed
                , viewCreditText ( lueTime, lueFadeout ) [ "lue", "for organizing", "Elm Game Jam #6" ] timePassed
                , viewCreditText ( lueFadeout + 1500, lueFadeout + 3500 ) [ "Textures:", "https://ambientcg.com", "https://cc0-textures.com" ] timePassed
                , viewCreditText ( lueFadeout + 4500, lueFadeout + 6500 ) [ "3D models & sounds:", "https://opengameart.org" ] timePassed
                , viewCreditText ( lueFadeout + 7500, lueFadeout + 9500 ) [ "Music:", "https://soundraw.io" ] timePassed
                , viewCreditText ( lueFadeout + 10500, lueFadeout + 12500 ) [ "Narration:", "https://murf.ai" ] timePassed
                , viewCreditText ( thxSongStartTime + 53000, thxSongStartTime + 58000 ) [ "Thanks for playing!" ] timePassed
                , div [ classList [ ( "credits-3d-scene", True ), ( "visible", timePassed >= evanTime && timePassed < lueFadeout ) ] ]
                    [ Scene3d.cloudy
                        { entities =
                            [ world
                                |> Physics.World.bodies
                                |> List.map
                                    (\body ->
                                        let
                                            frame3d =
                                                Physics.Body.frame body
                                                    |> Frame3d.copy
                                        in
                                        Physics.Body.data body
                                            |> Scene3d.placeIn frame3d
                                    )
                                |> Scene3d.group
                            ]
                        , camera =
                            Camera3d.perspective
                                { viewpoint =
                                    Viewpoint3d.orbit
                                        { focalPoint = Point3d.xyz (Length.meters 0.05) (Length.meters 0) (Length.meters 0.2)
                                        , groundPlane = SketchPlane3d.xy
                                        , azimuth =
                                            Angle.degrees
                                                (if timePassed >= sceneSpinningStartTime then
                                                    270 + frac ((timePassed - sceneSpinningStartTime) / 6000) * 360

                                                 else
                                                    270
                                                )
                                        , elevation = Angle.degrees 0
                                        , distance = Length.meters 0.3
                                        }
                                , verticalFieldOfView = Angle.degrees 90
                                }
                        , upDirection = Direction3d.z
                        , background = Scene3d.backgroundColor Color.black
                        , clipDepth = Length.centimeters 1
                        , dimensions = Tuple.mapBoth Pixels.int Pixels.int canvasSize
                        , visibility = Scene3d.fog (Length.meters 4)
                        }
                    ]
                ]


frac : Float -> Float
frac n =
    n - toFloat (truncate n)


viewCreditText : ( Float, Float ) -> List String -> Float -> Html msg
viewCreditText ( startTime, endTime ) texts time =
    texts
        |> List.map (text >> List.singleton >> Html.div [])
        |> Html.div [ classList [ ( "credits-text", True ), ( "visible", time >= startTime && time <= endTime ) ] ]



--orbit :
--    { focalPoint : Point3d units coordinates
--    , groundPlane : SketchPlane3d units coordinates defines
--    , azimuth : Angle
--    , elevation : Angle
--    , distance : Quantity Float units
--    }
