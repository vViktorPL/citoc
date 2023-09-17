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
        ]


type WorldCoordinates = WorldCoordinates

type MapCoordinates = MapCoordinates

-- MODEL

type alias Model =
    { textures: Textures
    , player: Player
    , level: Level
    }

type TurnControl = NoTurn | TurnLeft | TurnRight

type MoveControl = Stand | Forward | Backward

init : Textures -> Model
init textures =
    let
        level = LevelIndex.firstLevel
    in
    { textures = textures
    , player = Player.initOnLevel level
    , level = level
    }

-- UPDATE

type Msg
    = AnimationTick Float
    | KeyDown ControlKey
    | KeyUp ControlKey


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AnimationTick delta ->
            let
                newPlayer = Player.update delta model.player
                newSector =  (Player.getSector (Player.update (delta * 5) model.player) ) --Debug.log "pos"
            in
                if Level.collisionOnSector model.level newSector then
                    (model, Cmd.none)
                else
                    (handleTriggers model newPlayer, Cmd.none )

        KeyDown key ->
            case key of
                ControlTurnRight -> ({ model | player = Player.turnRight model.player }, Cmd.none)
                ControlTurnLeft -> ({ model | player = Player.turnLeft model.player }, Cmd.none)
                ControlForward -> ({ model | player = Player.walkForward model.player }, Cmd.none)
                ControlBackward -> ({ model | player = Player.walkBackward model.player }, Cmd.none)
                ControlStrafeLeft -> ({ model | player = Player.strafeLeft model.player }, Cmd.none)
                ControlStrafeRight -> ({ model | player = Player.strafeRight model.player }, Cmd.none)
                _ -> (model, Cmd.none)

        KeyUp key ->
            case key of
                ControlTurnRight -> ({ model | player = Player.stopTurning model.player }, Cmd.none)
                ControlTurnLeft -> ({ model | player = Player.stopTurning model.player }, Cmd.none)
                ControlForward -> ({ model | player = Player.stopWalkingForward model.player }, Cmd.none)
                ControlBackward -> ({ model | player = Player.stopWalkingBackward model.player }, Cmd.none)
                ControlStrafeLeft -> ({ model | player = Player.stopStrafingLeft model.player }, Cmd.none)
                ControlStrafeRight -> ({ model | player = Player.stopStrafingRight model.player }, Cmd.none)
                _ -> (model, Cmd.none)


type ControlKey
    = ControlForward
    | ControlBackward
    | ControlStrafeLeft
    | ControlStrafeRight
    | ControlTurnLeft
    | ControlTurnRight
    | Unknown

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
        maybeMovingOrientation =
            --Debug.log "movingOrientation"
            (if (dX == 0 && dY < 0) then Just North
            else if dX > 0 && dY == 0 then Just East
            else if dX == 0 && dY > 0 then Just South
            else if dX < 0 && dY == 0 then Just West
            else Nothing
            )

        lookingAt = (Player.getHorizontalOrientation newPlayer) --Debug.log "lookingAt"
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
                { model | player = newPlayer }



-- VIEW

view : Model -> Html Msg
view model =
    Scene3d.cloudy
        { entities = [ Level.view model.textures model.level ]
        , camera = Player.view model.player
        , upDirection = Direction3d.z
        , background = Scene3d.backgroundColor Color.white
        , clipDepth = Length.centimeters 1
        , dimensions = ( Pixels.int 800, Pixels.int 600 )
        }
