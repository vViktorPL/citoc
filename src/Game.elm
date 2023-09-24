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
import Level exposing (Level, Orientation(..), TriggerCondition(..), TriggerEffect(..))
import Level.Index as LevelIndex
import Textures exposing (Textures)
import Vector3d
import Dict exposing (Dict)
import Sound

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
    = Playing
    | FadingOutLevel Float
    | FadingInLevel Float

type alias Model =
    { textures: Textures
    , state: GameState
    , player: Player
    , level: Level
    , levelsLeft: List Level
    , counters: Dict String Int
    , gestureHistory: List Gesture
    , canvasSize : (Int, Int)
    }

type Gesture
    = LookLeft
    | LookRight
    | LookUp
    | LookDown

maxGestureHistory = 5

init : Textures -> Model
init textures =
    let
        level = LevelIndex.firstLevel
    in
    { textures = textures
    , state = Playing
    , player = Player.initOnLevel level
    , level = level
    , levelsLeft = LevelIndex.restLevels
    , counters = Dict.empty
    , gestureHistory = []
    , canvasSize = (800, 600)
    }


type Msg
    = AnimationTick Float
    | KeyDown ControlKey
    | KeyUp ControlKey
    | MouseMove (Int, Int)
    | WindowResize Int Int


fadeOutTime = 2000
fadeInTime = 1000

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
                Playing ->
                    let
                        newPlayer =
                            model.player
                                |> Player.update delta
                                |> Player.updatePlayerPosition v

                        v = (Player.getMovementVector model.player)
                            |> Vector3d.scaleBy delta
                            |> Level.applyCollision Player.playerRadius model.level (Player.getPlayerPosition model.player)
                    in
                        if playerCollides model.level newPlayer then
                            (model, Cmd.none)
                        else
                            let
                                (modelAfterTriggers, cmd) = handleTriggers model newPlayer
                            in
                                if playerCollides modelAfterTriggers.level modelAfterTriggers.player then
                                    ({ model | player = newPlayer }, Cmd.none)
                                else
                                    (modelAfterTriggers, cmd)

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
                            , state = FadingInLevel fadeInTime
                            }, Cmd.none)
                        else
                            ({ model | state = FadingOutLevel newTimeLeft, counters = Dict.empty }, Cmd.none)
                FadingInLevel timeLeft ->
                    let
                        newTimeLeft = max (timeLeft - delta) 0
                    in
                        if newTimeLeft == 0 then
                            ({ model | state = Playing }, Cmd.none)
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
    { model | state = FadingOutLevel fadeOutTime }

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
                )
                ({ model
                    | player = newPlayer
                    , gestureHistory =
                        if newX /= prevX || newY /= prevY then
                            []
                        else
                            model.gestureHistory
                }, Cmd.none)

view : Model -> Html Msg
view model =
    let
        opacity = case model.state of
            FadingOutLevel timeLeft -> timeLeft / fadeOutTime
            FadingInLevel timeLeft -> (fadeInTime - timeLeft) / fadeInTime
            Playing -> 1
    in
    Html.div [Html.Attributes.style "background" "white"] [
        Html.div [Html.Attributes.style "opacity" (String.fromFloat opacity) ]
            [ Scene3d.cloudy
                { entities = [ Level.view model.textures model.level ]
                --, sunlightDirection = Direction3d.z
                , camera = Player.view model.player
                , upDirection = Direction3d.z
                , background = Scene3d.backgroundColor Color.white
                , clipDepth = Length.centimeters 1
                , dimensions = Tuple.mapBoth Pixels.int Pixels.int model.canvasSize
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