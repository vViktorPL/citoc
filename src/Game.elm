module Game exposing (..)

import Html exposing (Html)
import Html.Attributes
import Scene3d
import Color exposing (..)
import Direction3d
import Length
import Pixels
import Browser.Events
import Json.Decode as Decode
import Player exposing (Player)
import Level exposing (Level, TriggerCondition(..), TriggerEffect(..))
import Orientation exposing (Orientation(..))
import Level.Index as LevelIndex
import Textures
import Vector3d
import Dict exposing (Dict)
import Sound
import Browser.Dom
import Task
import MeshCollection
import SceneAssets
import Frame3d
import Point3d
import Axis3d
import Angle
import Scene3d
import BreakableWall

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta AnimationTick
        , Browser.Events.onKeyDown (Decode.map KeyDown keyDecoder)
        , Browser.Events.onKeyUp (Decode.map KeyUp keyDecoder)
        , Browser.Events.onMouseMove (Decode.map MouseMove lockedMouseMovementDecoder)
        , Browser.Events.onResize WindowResize
        ]

type GameState
    = Initializing
    | Playing
    | FadingOutLevel Float
    | FadingInLevel Float

type alias Model =
    { state: GameState
    , player: Player
    , level: Level
    , levelsLeft: List Level
    , counters: Dict String Int
    , gestureHistory: List Gesture
    , canvasSize : (Int, Int)
    , fadeColor: String
    , backgroundColor: Color
    , visibility : Scene3d.Visibility
    }

type Gesture
    = LookLeft
    | LookRight
    | LookUp
    | LookDown

maxGestureHistory = 5

init : (Model, Cmd Msg)
init =
    let
        level = LevelIndex.firstLevel
    in
    ( { state = Initializing
        , player = Player.initOnLevel level
        , level = level
        , levelsLeft = LevelIndex.restLevels
        , counters = Dict.empty
        , gestureHistory = []
        , canvasSize = (800, 600)
        , fadeColor = "black"
        , backgroundColor = Color.white
        , visibility = Scene3d.clearView
      }
    , Cmd.batch
        [ Task.perform
            (\viewportDetails -> WindowResize (floor viewportDetails.viewport.width) (floor viewportDetails.viewport.height))
            Browser.Dom.getViewport
        , Sound.stopMusic ()
        ]
    )


type Msg
    = AnimationTick Float
    | KeyDown ControlKey
    | KeyUp ControlKey
    | MouseMove (Int, Int)
    | WindowResize Int Int


initFadeInTime = 3000
levelFadeOutTime = 2000
levelFadeInTime = 1000

playerCollides : Level -> Player -> Bool
playerCollides level player =
    case Level.cylinderCollisionSector level Player.playerRadius (Player.getPlayerPosition player) of
        Just _ -> True
        Nothing -> False

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WindowResize width height ->
            ({ model | canvasSize = (width, height) }, Cmd.none)

        AnimationTick delta ->
            case model.state of
                Initializing -> ({ model | state = FadingInLevel initFadeInTime }, Cmd.none)
                Playing ->
                    let
                        modelToUpdate = { model | level = Level.update delta model.level }
                        (newPlayer, playerCmd) =
                            model.player
                                |> Player.updatePlayerPosition v
                                |> Player.update delta

                        v = (Player.getMovementVector model.player)
                            |> Vector3d.scaleBy delta
                            |> Level.applyCollision Player.playerRadius model.level (Player.getPlayerPosition model.player)
                    in
                        if playerCollides model.level newPlayer then
                            ({ modelToUpdate | player = model.player }, Cmd.none)
                        else
                            let
                                (modelAfterTriggers, cmd) = handleTriggers modelToUpdate newPlayer
                            in
                                if playerCollides modelAfterTriggers.level modelAfterTriggers.player then
                                    ({ modelToUpdate | player = newPlayer }, playerCmd)
                                else
                                    (modelAfterTriggers, Cmd.batch [cmd, playerCmd])

                FadingOutLevel timeLeft ->
                    let
                        newTimeLeft = max (timeLeft - delta) 0
                        nextLevel = model.levelsLeft
                            |> List.head
                            |> Maybe.withDefault model.level
                    in
                        if newTimeLeft == 0 then
                            ({ model
                            | level = nextLevel
                            , levelsLeft = List.drop 1 model.levelsLeft
                            , player = Player.initOnLevel nextLevel
                            , state = FadingInLevel levelFadeInTime
                            }, Cmd.none)
                        else
                            ({ model | state = FadingOutLevel newTimeLeft, counters = Dict.empty }, Cmd.none)
                FadingInLevel timeLeft ->
                    let
                        newTimeLeft = max (timeLeft - delta) 0
                    in
                        if newTimeLeft == 0 then
                            ({ model | state = Playing, fadeColor = "white" }, Cmd.none)
                        else
                            ({ model | state = FadingInLevel newTimeLeft }, Cmd.none)

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

transitionToNextLevel : Model -> Model
transitionToNextLevel model =
    { model | state = FadingOutLevel levelFadeOutTime }

handleTriggers : Model -> Player -> (Model, Cmd Msg)
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

                          LookingAtGround ->
                              (Angle.inDegrees (Player.getVerticalLookAngle newPlayer)) <= -45

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

                          StepIn ->
                              True

                          CounterEquals counterName desiredNumber ->
                              let
                                  counterState = Maybe.withDefault 0 (Dict.get counterName model.counters)
                              in
                                  desiredNumber == counterState
                      )
                      trigger.conditions
                  )
              |> List.concatMap (.effects)
              |> List.foldl
                (\effect (modelAcc, cmdAcc) ->
                    case effect of
                        Teleport targetSector ->
                            ({ modelAcc | player = Player.teleport modelAcc.player targetSector }, cmdAcc)
                        NextLevel ->
                            (transitionToNextLevel modelAcc, Cmd.batch [cmdAcc, Sound.playSound "success.mp3"])
                        ChangeTile sector newTile ->
                            ({ modelAcc | level = Level.updateTile sector newTile modelAcc.level }, cmdAcc)
                        CreateTrigger trigger ->
                            ({ modelAcc | level = Level.addTrigger trigger modelAcc.level }, cmdAcc)
                        RemoveAllTriggersInSector sector ->
                            ({ modelAcc | level = Level.removeAllTriggersAtSector sector modelAcc.level }, cmdAcc)
                        IncrementCounter counterName ->
                            ({ modelAcc | counters = Dict.update counterName (\prevCount -> Just (Maybe.withDefault 0 prevCount + 1)) modelAcc.counters }
                            , Cmd.batch [cmdAcc, Sound.playSound "notify-up.mp3"]
                            )
                        DecrementCounter counterName ->
                            ({ modelAcc | counters = Dict.update counterName (\prevCount -> Just (Maybe.withDefault 0 prevCount - 1)) modelAcc.counters }
                            , Cmd.batch [cmdAcc, Sound.playSound "notify-down.mp3"]
                            )
                        PlaySound fileName ->
                            (modelAcc, Cmd.batch [cmdAcc, Sound.playSound fileName])
                        InitFog color distance ->
                            ({ modelAcc | backgroundColor = color, visibility = Scene3d.fog distance }, cmdAcc)
                        SitDown ->
                            ({ modelAcc | player = Player.sitDown modelAcc.player }, cmdAcc)
                        OpenTerms ->
                            ({ modelAcc | level = Level.openTerms modelAcc.level }, Cmd.batch [cmdAcc, Sound.playSound "elevator_door.mp3"])
                        PlayMusic fileName ->
                            (modelAcc, Cmd.batch [cmdAcc, Sound.playMusic fileName ])
                )
                ({ model
                    | player = newPlayer
                    , gestureHistory =
                        if newX /= prevX || newY /= prevY then
                            []
                        else
                            model.gestureHistory
                }, Cmd.none)

view : SceneAssets.Model -> Model -> Html Msg
view sceneAssets model =
    let
        opacity = case model.state of
            FadingOutLevel timeLeft -> timeLeft / levelFadeOutTime
            FadingInLevel timeLeft -> (levelFadeInTime - timeLeft) / levelFadeInTime
            Playing -> 1
            Initializing -> 0

    in
    Html.div [Html.Attributes.style "background" model.fadeColor] [
        Html.div
            [ Html.Attributes.style "opacity" (String.fromFloat opacity)
            , Html.Attributes.style "visibility" (if model.state == Initializing then "hidden" else "visible")
            ]
            [ Scene3d.cloudy
                { entities =
                    [ Level.view sceneAssets model.level
                    , SceneAssets.breakableWall sceneAssets
                        |> Maybe.map BreakableWall.view
                        |> Maybe.withDefault Scene3d.nothing

                    --, SceneAssets.castle sceneAssets
                    --    |> Scene3d.rotateAround (Axis3d.x) (Angle.degrees 180)
                    --    |> Scene3d.scaleAbout Point3d.origin 0.2
                    --    --|> Scene3d.placeIn (Frame3d.atPoint (Point3d.meters -12 32 9))
                    --
                    --    |> Scene3d.placeIn (Frame3d.atPoint (Point3d.meters -17 35 5.4))
                    ]
                    --, SceneAssets.castleSkybox sceneAssets
                    --    |> Scene3d.placeIn (Frame3d.atPoint (Player.getPlayerPosition model.player)) ]
                --, sunlightDirection = Direction3d.z
                , camera = Player.view model.player
                , upDirection = Direction3d.z
                , background = Scene3d.backgroundColor model.backgroundColor
                , clipDepth = Length.centimeters 1
                , dimensions = Tuple.mapBoth Pixels.int Pixels.int model.canvasSize
                , visibility = model.visibility
                }
            ]
    ]

--{ upDirection : Direction3d coordinates
  --    , sunlightDirection : Direction3d coordinates
  --    , shadows : Bool
  --    , dimensions : ( Quantity Int Pixels, Quantity Int Pixels )
  --    , camera : Camera3d Meters coordinates
  --    , clipDepth : Length
  --    , background : Background coordinates
  --    , entities : List (Entity coordinates)
  --    }
