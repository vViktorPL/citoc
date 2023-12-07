module Menu exposing (..)

import Angle
import Assets
import Browser.Dom
import Browser.Events
import Camera3d
import Color
import Direction3d
import Frame3d
import Html exposing (Html)
import Html.Attributes exposing (class, style)
import Html.Events
import Length
import LevelTile
import Pixels
import Point3d
import Scene3d
import Sound
import Task
import Vector3d
import Viewpoint3d


type alias Model =
    { initialized : Bool
    , canvasSize : ( Int, Int )
    , offset : Float
    , state : MenuState
    , backward : Bool
    }


type Msg
    = AnimationTick Float
    | WindowResize Int Int
    | NewGamePositionActivated


type MenuState
    = Idle
    | StartingNewGame Float


type OutMsg
    = Noop
    | StartNewGame


dependencies : List Assets.Dependency
dependencies =
    let
        tileDeps =
            [ LevelTile.floor, LevelTile.wall ]
                |> List.concatMap LevelTile.dependencies
    in
    tileDeps ++ [ Assets.MusicDep "menu.mp3", Assets.MusicDep "menu2.mp3" ]


init : Bool -> ( Model, Cmd Msg )
init backward =
    ( { initialized = False
      , canvasSize = ( 800, 600 )
      , offset = 0
      , state = Idle
      , backward = backward
      }
    , Cmd.batch
        [ Task.perform
            (\viewportDetails -> WindowResize (floor viewportDetails.viewport.width) (floor viewportDetails.viewport.height))
            Browser.Dom.getViewport
        , Sound.playMusic
            (if backward then
                "menu2.mp3"

             else
                "menu.mp3"
            )
        ]
    )


update : Msg -> Model -> ( Model, OutMsg )
update msg model =
    case msg of
        AnimationTick delta ->
            let
                newOffset =
                    model.offset
                        + delta
                        * 0.0001
                        * (if model.backward then
                            -1

                           else
                            1
                          )

                safeNewOffset =
                    newOffset - toFloat (floor newOffset)

                animatedModel =
                    { model | offset = safeNewOffset, initialized = True }
            in
            case model.state of
                Idle ->
                    ( animatedModel, Noop )

                StartingNewGame remainingFadeout ->
                    let
                        newFadeout =
                            max 0 (remainingFadeout - delta * 0.002)
                    in
                    ( { animatedModel | state = StartingNewGame newFadeout }
                    , if newFadeout == 0 then
                        StartNewGame

                      else
                        Noop
                    )

        WindowResize width height ->
            ( { model | canvasSize = ( width, height ) }, Noop )

        NewGamePositionActivated ->
            ( { model | state = StartingNewGame 1.0 }, Noop )


view : Assets.Model -> Model -> Html Msg
view assets model =
    let
        opacity =
            String.fromFloat
                (case model.state of
                    StartingNewGame fadeOut ->
                        fadeOut

                    _ ->
                        1
                )

        segments =
            List.range 0 40
                |> List.map
                    (\index ->
                        let
                            currentSegmentOffset =
                                -(toFloat index) + model.offset

                            position =
                                Point3d.meters 0 currentSegmentOffset 0
                        in
                        [ LevelTile.view assets LevelTile.floor
                        , LevelTile.view assets LevelTile.wall |> Scene3d.translateBy (Vector3d.fromMeters { x = 1, y = 0, z = 0 })
                        , LevelTile.view assets LevelTile.wall |> Scene3d.translateBy (Vector3d.fromMeters { x = -1, y = 0, z = 0 })
                        ]
                            |> Scene3d.group
                            |> Scene3d.placeIn (Frame3d.atPoint position)
                    )
    in
    Html.div [ class "mainMenuContainer", style "opacity" opacity ]
        [ Html.div []
            [ Html.text
                (if model.initialized then
                    ""

                 else
                    "Initializing..."
                )
            ]
        , Scene3d.cloudy
            { entities = segments
            , camera = camera
            , upDirection = Direction3d.z
            , background = Scene3d.backgroundColor Color.white
            , clipDepth = Length.centimeters 1
            , dimensions = Tuple.mapBoth Pixels.int Pixels.int model.canvasSize
            , visibility = Scene3d.fog (Length.meters 4)
            }
        , Html.div
            [ class "mainMenu"
            , style "visibility"
                (if model.initialized then
                    "visible"

                 else
                    "hidden"
                )
            ]
            [ Html.div [ class "logo" ] []
            , Html.div [ class "menuPosition", Html.Events.onClick NewGamePositionActivated ] [ Html.text "New game" ]
            , Html.div [ class "menuPosition", class "disabled" ] [ Html.text "Settings" ]
            , Html.div [ class "menuPosition", class "disabled" ] [ Html.text "About" ]
            ]
        ]


camera =
    Camera3d.perspective
        { viewpoint =
            Viewpoint3d.lookAt
                { eyePoint = Point3d.xyz (Length.meters 0) (Length.meters 0.5) (Length.meters 0.5)
                , focalPoint = Point3d.xyz (Length.meters 0) (Length.meters -1) (Length.meters 0.5)
                , upDirection = Direction3d.positiveZ
                }
        , verticalFieldOfView = Angle.degrees 110
        }


subscription : Model -> Sub Msg
subscription menu =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta AnimationTick
        , Browser.Events.onResize WindowResize
        ]
