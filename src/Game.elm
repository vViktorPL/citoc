module Game exposing (..)

import Html exposing (Html)
import Browser
import Scene3d
import Color exposing (..)
import Direction3d
import Angle
import Length
import Pixels
import Camera3d
import Viewpoint3d
import Point3d
import Scene3d.Material
import Block3d
import Browser.Events
import Json.Decode as Decode
import Player exposing (Player)
import Level exposing (Level, Orientation(..), TriggerCondition(..), TriggerEffect(..))
import Level.Index as LevelIndex
import Scene3d.Material as Material
import Task
import WebGL.Texture
import Dict exposing (Dict)
import Textures exposing (Textures)

-- MAIN

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta AnimationTick
        , Browser.Events.onKeyDown (Decode.map KeyDown keyDecoder)
        , Browser.Events.onKeyUp (Decode.map KeyUp keyDecoder)
        , Browser.Events.onMouseMove (Decode.map MouseMove lockedMouseMovementDecoder)
        , Browser.Events.onResize WindowResize
        ]



type WorldCoordinates = WorldCoordinates

type MapCoordinates = MapCoordinates

-- MODEL

type alias Model =
    { textures: Textures
    , player: Player
    , level: Level
    , gestureHistory: List Gesture
    , canvasSize : (Int, Int)
    }

type Gesture
    = LookLeft
    | LookRight
    | LookUp
    | LookDown

type TurnControl = NoTurn | TurnLeft | TurnRight

type MoveControl = Stand | Forward | Backward

maxGestureHistory = 5

init : Textures -> Model
init textures =
    let
        level = LevelIndex.firstLevel
    in
    { textures = textures
    , player = Player.initOnLevel level
    , level = level
    , gestureHistory = []
    , canvasSize = (800, 600)
    }

-- UPDATE

type Msg
    = AnimationTick Float
    | KeyDown ControlKey
    | KeyUp ControlKey
    | MouseMove (Int, Int)
    | WindowResize Int Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WindowResize width height ->
            ({ model | canvasSize = (width, height) }, Cmd.none)

        AnimationTick delta ->
            let
                newPlayer = Player.update delta model.player
                newSector = (Player.getSector (Player.update (delta * 5) model.player) ) --Debug.log "pos"
            in
                if Level.collisionOnSector model.level newSector then
                    (model, Cmd.none)
                else
                    (handleTriggers model newPlayer, Cmd.none )

        KeyDown key ->
            ({ model
                | player = controlKeyToPlayerAction key model.player
                , gestureHistory = case key of
                    ControlTurnLeft -> LookLeft :: List.take (maxGestureHistory - 1) model.gestureHistory
                    ControlTurnRight -> LookRight :: List.take (maxGestureHistory - 1) model.gestureHistory
                    _ -> model.gestureHistory
            }, Cmd.none )

        KeyUp key ->
            case key of
                ControlTurnRight -> ({ model | player = Player.stopTurning model.player }, Cmd.none)
                ControlTurnLeft -> ({ model | player = Player.stopTurning model.player }, Cmd.none)
                ControlForward -> ({ model | player = Player.stopWalkingForward model.player }, Cmd.none)
                ControlBackward -> ({ model | player = Player.stopWalkingBackward model.player }, Cmd.none)
                ControlStrafeLeft -> ({ model | player = Player.stopStrafingLeft model.player }, Cmd.none)
                ControlStrafeRight -> ({ model | player = Player.stopStrafingRight model.player }, Cmd.none)
                _ -> (model, Cmd.none)

        MouseMove (x, y) ->
            let
                horizontalMove = abs y < abs x

                maybeGesture =
                    if horizontalMove && x > 2 then
                        Just LookRight
                    else if horizontalMove && x < -2 then
                        Just LookLeft
                    else if not horizontalMove && y > 2 then
                        Just LookDown
                    else if not horizontalMove && y < -2 then
                        Just LookUp
                    else
                        Nothing
            in
                ({ model
                    | player = Player.updateLookByMouseMovement (x, y) model.player
                    , gestureHistory =
                        case (maybeGesture, maybeGesture /= List.head model.gestureHistory) of
                            (Just gesture, True) ->
                                gesture :: List.take (maxGestureHistory - 1) model.gestureHistory
                            _ -> model.gestureHistory
                }, Cmd.none )



controlKeyToPlayerAction : ControlKey -> (Player -> Player)
controlKeyToPlayerAction key =
    case key of
        ControlTurnLeft -> Player.turnLeft
        ControlTurnRight -> Player.turnRight
        ControlForward -> Player.walkForward
        ControlBackward -> Player.walkBackward
        ControlStrafeLeft -> Player.strafeLeft
        ControlStrafeRight -> Player.strafeRight
        Unknown -> identity


type ControlKey
    = ControlForward
    | ControlBackward
    | ControlStrafeLeft
    | ControlStrafeRight
    | ControlTurnLeft
    | ControlTurnRight
    | Unknown

lockedMouseMovementDecoder : Decode.Decoder (Int, Int)
lockedMouseMovementDecoder =
    Decode.at ["view", "document", "pointerLockElement"] (Decode.nullable Decode.value)
        |> Decode.andThen
            (\lockElement ->
                case lockElement of
                    Just _ ->
                        Decode.map2 Tuple.pair
                            (Decode.field "movementX" Decode.int)
                            (Decode.field "movementY" Decode.int)

                    Nothing -> Decode.fail "Mouse not locked"
            )


keyDecoder : Decode.Decoder ControlKey
keyDecoder =
    Decode.field "key" Decode.string
        |> Decode.map
            (\key ->
                case key of
                    "ArrowLeft" -> ControlTurnLeft
                    "ArrowRight" -> ControlTurnRight
                    "ArrowUp" -> ControlForward
                    "ArrowDown" -> ControlBackward
                    "w" -> ControlForward
                    "W" -> ControlForward
                    "a" -> ControlStrafeLeft
                    "A" -> ControlStrafeLeft
                    "s" -> ControlBackward
                    "S" -> ControlBackward
                    "d" -> ControlStrafeRight
                    "D" -> ControlStrafeRight
                    _ -> Unknown
            )


handleTriggers : Model -> Player -> Model
handleTriggers model newPlayer =
    let
        (prevX, prevY) = Player.getSector model.player
        (newX, newY) = Player.getSector newPlayer
        (dX, dY) =  (prevX - newX, prevY - newY)
        lookingAt = (Player.getHorizontalOrientation newPlayer)
    in
        Level.getTriggersAt model.level (newX, newY)
              |> List.filter
                  (\trigger -> List.all
                      (\condition -> case condition of
                          EnteredFrom orientation ->
                              case orientation of
                                  North -> dY < 0
                                  South -> dY > 0
                                  East -> dX > 0
                                  West -> dX < 0

                          LookAngle orientation ->
                              orientation == lookingAt

                          NegativeHeadshake ->
                              let
                                  last3Gestures = List.take 3 model.gestureHistory
                              in
                                  last3Gestures == [LookLeft, LookRight, LookLeft] ||
                                  last3Gestures == [LookRight, LookLeft, LookRight]

                          Nod ->
                              let
                                  last3Gestures = List.take 3 model.gestureHistory
                              in
                                  last3Gestures == [LookUp, LookDown, LookUp] ||
                                  last3Gestures == [LookDown, LookUp, LookDown]

                      )
                      trigger.conditions
                  )
              |> List.concatMap (.effects)
              |> List.foldl
                (\effect modelAcc ->
                    case effect of
                        Teleport targetSector ->
                            { modelAcc | player = Player.teleport modelAcc.player targetSector }
                )
                { model
                    | player = newPlayer
                    , gestureHistory =
                        if newX /= prevX || newY /= prevY then
                            []
                        else
                            model.gestureHistory
                }



-- VIEW

view : Model -> Html Msg
view model =
    Scene3d.cloudy
        { entities = [ Level.view model.textures model.level ]
        , camera = Player.view model.player
        , upDirection = Direction3d.z
        , background = Scene3d.backgroundColor Color.white
        , clipDepth = Length.centimeters 1
        , dimensions = Tuple.mapBoth Pixels.int Pixels.int model.canvasSize
        }
