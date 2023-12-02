module Game exposing (..)

import Angle
import Browser.Dom
import Browser.Events
import Color exposing (..)
import Dict exposing (Dict)
import Direction3d
import Ending
import Html exposing (Html)
import Html.Attributes
import Json.Decode as Decode
import Length
import Level exposing (Level, TriggerCondition(..), TriggerEffect(..))
import Level.Index as LevelIndex
import Narration
import Orientation exposing (Orientation(..))
import Pixels
import Player exposing (Player)
import Point3d
import Scene3d
import SceneAssets
import Sound
import Task
import Vector3d
import WebBrowser


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta AnimationTick
        , Browser.Events.onKeyDown (Decode.map KeyDown keyDecoder)
        , Browser.Events.onKeyUp (Decode.map KeyUp keyDecoder)
        , Browser.Events.onMouseMove (Decode.map MouseMove lockedMouseMovementDecoder)
        , Browser.Events.onResize WindowResize
        , WebBrowser.windowShake BrowserWindowShaken
        , WebBrowser.clipboardEvent Clipboard
        ]


type GameState
    = Initializing
    | Playing
    | FadingOutLevel Float
    | FadingInLevel Float
    | FinalFadeOut Float
    | GameEnding Ending.Model


type alias Model =
    { state : GameState
    , player : Player
    , level : Level
    , levelsLeft : List Level
    , counters : Dict String Int
    , gestureHistory : List Gesture
    , canvasSize : ( Int, Int )
    , fadeColor : String
    , backgroundColor : Color
    , visibility : Scene3d.Visibility
    , narration : Narration.Model
    , sceneAssets : SceneAssets.Model
    }


type Gesture
    = LookLeft
    | LookRight
    | LookUp
    | LookDown


maxGestureHistory =
    5


init : SceneAssets.Model -> ( Model, Cmd Msg )
init sceneAssets =
    let
        level =
            --LevelIndex.restLevels
            --    |> List.drop 1
            --    |> List.head
            --    |> Maybe.withDefault
            LevelIndex.firstLevel
    in
    ( { state = Initializing
      , player = Player.initOnLevel level
      , level = level
      , levelsLeft = LevelIndex.restLevels
      , counters = Dict.empty
      , gestureHistory = []
      , canvasSize = ( 800, 600 )
      , fadeColor = "black"
      , backgroundColor = Color.white
      , visibility = Scene3d.clearView
      , narration = Narration.init
      , sceneAssets = sceneAssets
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
    | MouseMove ( Int, Int )
    | WindowResize Int Int
    | BrowserWindowShaken
    | Clipboard WebBrowser.ClipboardEvent


initFadeInTime =
    3000


levelFadeOutTime =
    2000


levelFadeInTime =
    1000


playerCollides : Level -> Player -> Bool
playerCollides level player =
    case Level.cylinderCollisionSector level (Player.isUpsideDown player) Player.playerRadius (Player.getPlayerPosition player) of
        Just _ ->
            True

        Nothing ->
            False


playerTriggerInteraction : Model -> Player -> ( Model, Cmd Msg )
playerTriggerInteraction model player =
    let
        ( modelAfterTriggers, cmd ) =
            handleTriggers model player
    in
    if playerCollides modelAfterTriggers.level modelAfterTriggers.player then
        ( { model | player = player }, cmd )

    else
        ( modelAfterTriggers, cmd )


handleStepSounds : Level.Level -> Player.Player -> Player.OutMsg -> Cmd msg
handleStepSounds level player outmsg =
    case outmsg of
        Player.EmitStepSound soundNumber ->
            case Level.getGroundSound level (Player.getSector player) of
                Level.SolidFloor ->
                    Sound.playSound ("step-" ++ String.fromInt soundNumber ++ ".mp3")

                Level.SandGround ->
                    Sound.playSound "sand-step.mp3"

                _ ->
                    Cmd.none

        _ ->
            Cmd.none


updateAnimation : Float -> Model -> ( Model, Cmd Msg )
updateAnimation delta model =
    case model.state of
        Initializing ->
            ( { model | state = FadingInLevel initFadeInTime }, Cmd.none )

        Playing ->
            let
                modelToUpdate =
                    { model | level = Level.update delta model.level }

                ( newPlayer, playerOutMsg ) =
                    model.player
                        |> Player.updatePlayerPosition v
                        |> Player.update delta

                playerCmd =
                    handleStepSounds modelToUpdate.level newPlayer playerOutMsg

                v =
                    Player.getMovementVector model.player
                        |> Vector3d.scaleBy delta

                ( interactionResult, interactionCmd ) =
                    Level.interactAsCylinder (Player.isUpsideDown model.player) Player.playerRadius (Player.getPlayerPosition model.player) v modelToUpdate.level
            in
            case interactionResult of
                Level.LevelUpdated updatedLevel ->
                    ( { modelToUpdate | level = updatedLevel }, interactionCmd )

                --player = newPlayer,
                Level.LevelCollision adjustedVector ->
                    let
                        ( adjustedPlayer, adjustedOutMsg ) =
                            model.player
                                |> Player.updatePlayerPosition adjustedVector
                                |> Player.update delta

                        adjustedCmd =
                            handleStepSounds postTriggerModel.level adjustedPlayer adjustedOutMsg

                        ( postTriggerModel, triggerCmd ) =
                            playerTriggerInteraction modelToUpdate adjustedPlayer
                    in
                    ( postTriggerModel, Cmd.batch [ adjustedCmd, interactionCmd, triggerCmd ] )

                Level.NoInteraction ->
                    playerTriggerInteraction modelToUpdate newPlayer
                        |> Tuple.mapSecond (\triggerCmd -> Cmd.batch [ playerCmd, interactionCmd, triggerCmd ])

        FadingOutLevel timeLeft ->
            let
                newTimeLeft =
                    max (timeLeft - delta) 0

                nextLevel =
                    model.levelsLeft
                        |> List.head
                        |> Maybe.withDefault model.level
            in
            if newTimeLeft == 0 then
                ( { model
                    | level = nextLevel
                    , levelsLeft = List.drop 1 model.levelsLeft
                    , player = Player.initOnLevel nextLevel
                    , state = FadingInLevel levelFadeInTime
                  }
                , Cmd.none
                )

            else
                ( { model | state = FadingOutLevel newTimeLeft, counters = Dict.empty }, Cmd.none )

        FinalFadeOut timeLeft ->
            let
                newTimeLeft =
                    max (timeLeft - delta) 0
            in
            if timeLeft == 0 then
                ( { model | state = GameEnding (Ending.init model.sceneAssets) }, Cmd.none )

            else
                ( { model | state = FinalFadeOut newTimeLeft }, Cmd.none )

        FadingInLevel timeLeft ->
            let
                newTimeLeft =
                    max (timeLeft - delta) 0
            in
            if newTimeLeft == 0 then
                ( { model | state = Playing, fadeColor = "white" }, Cmd.none )

            else
                ( { model | state = FadingInLevel newTimeLeft }, Cmd.none )

        GameEnding ending ->
            let
                ( updatedEnding, endingCmd ) =
                    Ending.update delta ending
            in
            ( { model | state = GameEnding updatedEnding }, endingCmd )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WindowResize width height ->
            ( { model | canvasSize = ( width, height ) }, Cmd.none )

        AnimationTick delta ->
            let
                ( animatedModel, animationCmd ) =
                    updateAnimation delta model

                ( updatedNarration, narrationCmd ) =
                    Narration.update delta animatedModel.narration
            in
            ( { animatedModel | narration = updatedNarration }, Cmd.batch [ animationCmd, narrationCmd ] )

        Clipboard _ ->
            ( model, Cmd.none )

        KeyDown key ->
            ( { model
                | player = controlKeyToPlayerAction key model.player
                , gestureHistory =
                    case key of
                        ControlTurnLeft ->
                            LookLeft :: List.take (maxGestureHistory - 1) model.gestureHistory

                        ControlTurnRight ->
                            LookRight :: List.take (maxGestureHistory - 1) model.gestureHistory

                        _ ->
                            model.gestureHistory
              }
            , Cmd.none
            )

        KeyUp key ->
            case key of
                ControlTurnRight ->
                    ( { model | player = Player.stopTurning model.player }, Cmd.none )

                ControlTurnLeft ->
                    ( { model | player = Player.stopTurning model.player }, Cmd.none )

                ControlForward ->
                    ( { model | player = Player.stopWalkingForward model.player }, Cmd.none )

                ControlBackward ->
                    ( { model | player = Player.stopWalkingBackward model.player }, Cmd.none )

                ControlStrafeLeft ->
                    ( { model | player = Player.stopStrafingLeft model.player }, Cmd.none )

                ControlStrafeRight ->
                    ( { model | player = Player.stopStrafingRight model.player }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        MouseMove ( x, y ) ->
            let
                horizontalMove =
                    abs y < abs x

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
            ( { model
                | player = Player.updateLookByMouseMovement ( x, y ) model.player
                , gestureHistory =
                    case ( maybeGesture, maybeGesture /= List.head model.gestureHistory ) of
                        ( Just gesture, True ) ->
                            gesture :: List.take (maxGestureHistory - 1) model.gestureHistory

                        _ ->
                            model.gestureHistory
              }
            , Cmd.none
            )

        BrowserWindowShaken ->
            case model.state of
                Playing ->
                    let
                        effectsToExecute =
                            Level.getAllTriggers model.level
                                |> List.concatMap
                                    (\{ conditions, effects } ->
                                        if conditions == [ Level.WindowShake ] then
                                            effects

                                        else
                                            []
                                    )
                    in
                    if List.length effectsToExecute > 0 then
                        executeEffects model effectsToExecute

                    else
                        ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )


controlKeyToPlayerAction : ControlKey -> (Player -> Player)
controlKeyToPlayerAction key =
    case key of
        ControlTurnLeft ->
            Player.turnLeft

        ControlTurnRight ->
            Player.turnRight

        ControlForward ->
            Player.walkForward

        ControlBackward ->
            Player.walkBackward

        ControlStrafeLeft ->
            Player.strafeLeft

        ControlStrafeRight ->
            Player.strafeRight

        Unknown ->
            identity


type ControlKey
    = ControlForward
    | ControlBackward
    | ControlStrafeLeft
    | ControlStrafeRight
    | ControlTurnLeft
    | ControlTurnRight
    | Unknown


lockedMouseMovementDecoder : Decode.Decoder ( Int, Int )
lockedMouseMovementDecoder =
    Decode.at [ "view", "document", "pointerLockElement" ] (Decode.nullable Decode.value)
        |> Decode.andThen
            (\lockElement ->
                case lockElement of
                    Just _ ->
                        Decode.map2 Tuple.pair
                            (Decode.field "movementX" Decode.int)
                            (Decode.field "movementY" Decode.int)

                    Nothing ->
                        Decode.fail "Mouse not locked"
            )


keyDecoder : Decode.Decoder ControlKey
keyDecoder =
    Decode.field "key" Decode.string
        |> Decode.map
            (\key ->
                case key of
                    "ArrowLeft" ->
                        ControlTurnLeft

                    "ArrowRight" ->
                        ControlTurnRight

                    "ArrowUp" ->
                        ControlForward

                    "ArrowDown" ->
                        ControlBackward

                    "w" ->
                        ControlForward

                    "W" ->
                        ControlForward

                    "a" ->
                        ControlStrafeLeft

                    "A" ->
                        ControlStrafeLeft

                    "s" ->
                        ControlBackward

                    "S" ->
                        ControlBackward

                    "d" ->
                        ControlStrafeRight

                    "D" ->
                        ControlStrafeRight

                    _ ->
                        Unknown
            )


transitionToNextLevel : Model -> Model
transitionToNextLevel model =
    { model | state = FadingOutLevel levelFadeOutTime }


handleTriggers : Model -> Player -> ( Model, Cmd Msg )
handleTriggers model newPlayer =
    let
        ( prevX, prevY ) =
            Player.getSector model.player

        ( newX, newY ) =
            Player.getSector newPlayer

        ( dX, dY ) =
            ( prevX - newX, prevY - newY )

        lookingAt =
            Player.getHorizontalOrientation newPlayer
    in
    Level.getTriggersAt model.level ( newX, newY )
        |> List.filter
            (\trigger ->
                List.all
                    (\condition ->
                        case condition of
                            EnteredFrom orientation ->
                                case orientation of
                                    North ->
                                        dY < 0

                                    South ->
                                        dY > 0

                                    East ->
                                        dX > 0

                                    West ->
                                        dX < 0

                            LookAngle orientation ->
                                orientation == lookingAt

                            LookingAtGround ->
                                Angle.inDegrees (Player.getVerticalLookAngle newPlayer) <= -45

                            NegativeHeadshake ->
                                let
                                    last3Gestures =
                                        List.take 3 model.gestureHistory
                                in
                                last3Gestures
                                    == [ LookLeft, LookRight, LookLeft ]
                                    || last3Gestures
                                    == [ LookRight, LookLeft, LookRight ]

                            Nod ->
                                let
                                    last3Gestures =
                                        List.take 3 model.gestureHistory
                                in
                                last3Gestures
                                    == [ LookUp, LookDown, LookUp ]
                                    || last3Gestures
                                    == [ LookDown, LookUp, LookDown ]

                            StepIn ->
                                dX /= 0 || dY /= 0

                            CameBackToFloor ->
                                Player.isUpsideDown model.player && not (Player.isUpsideDown newPlayer)

                            CounterEquals counterName desiredNumber ->
                                let
                                    counterState =
                                        Maybe.withDefault 0 (Dict.get counterName model.counters)
                                in
                                desiredNumber == counterState

                            WindowShake ->
                                False
                    )
                    trigger.conditions
            )
        |> List.concatMap .effects
        |> executeEffects
            { model
                | player = newPlayer
                , gestureHistory =
                    if newX /= prevX || newY /= prevY then
                        []

                    else
                        model.gestureHistory
            }


executeEffects : Model -> List TriggerEffect -> ( Model, Cmd Msg )
executeEffects model effects =
    effects
        |> List.foldl
            (\effect ( modelAcc, cmdAcc ) ->
                case effect of
                    Teleport targetSector ->
                        ( { modelAcc | player = Player.seamlessTeleport modelAcc.player targetSector }, cmdAcc )

                    NextLevel ->
                        ( transitionToNextLevel modelAcc, Cmd.batch [ cmdAcc, Sound.playSound "success.mp3" ] )

                    ChangeTile sector newTile ->
                        ( { modelAcc | level = Level.updateTile sector newTile modelAcc.level }, cmdAcc )

                    CreateTrigger trigger ->
                        ( { modelAcc | level = Level.addTrigger trigger modelAcc.level }, cmdAcc )

                    RemoveAllTriggersInSector sector ->
                        ( { modelAcc | level = Level.removeAllTriggersAtSector sector modelAcc.level }, cmdAcc )

                    RemoveAllTriggersInSectors sectors ->
                        ( { modelAcc | level = Level.removeAllTriggersAtSectors sectors modelAcc.level }, cmdAcc )

                    IncrementCounter counterName ->
                        ( { modelAcc | counters = Dict.update counterName (\prevCount -> Just (Maybe.withDefault 0 prevCount + 1)) modelAcc.counters }
                        , Cmd.batch [ cmdAcc, Sound.playSound "notify-up.mp3" ]
                        )

                    DecrementCounter counterName ->
                        ( { modelAcc | counters = Dict.update counterName (\prevCount -> Just (Maybe.withDefault 0 prevCount - 1)) modelAcc.counters }
                        , Cmd.batch [ cmdAcc, Sound.playSound "notify-down.mp3" ]
                        )

                    PlaySound fileName ->
                        ( modelAcc, Cmd.batch [ cmdAcc, Sound.playSound fileName ] )

                    InitFog color distance ->
                        ( { modelAcc | backgroundColor = color, visibility = Scene3d.fog distance }, cmdAcc )

                    SitDown ->
                        ( { modelAcc | player = Player.sitDown modelAcc.player }, cmdAcc )

                    OpenTerms sector ->
                        ( { modelAcc | level = Level.openTerms modelAcc.level sector }, Cmd.batch [ cmdAcc, Sound.playSound "elevator_door.mp3" ] )

                    PlayMusic fileName ->
                        ( modelAcc, Cmd.batch [ cmdAcc, Sound.playMusic fileName ] )

                    StartNarration narrationNumber ->
                        let
                            ( narration, narrationCmd ) =
                                Narration.playNarration modelAcc.narration narrationNumber
                        in
                        ( { modelAcc | narration = narration }, Cmd.batch [ cmdAcc, narrationCmd ] )

                    ShowGameEndingScreen ->
                        ( { modelAcc | state = FinalFadeOut levelFadeOutTime, fadeColor = "black" }, Cmd.batch [ cmdAcc, Sound.stopMusic () ] )

                    BreakWall sector ->
                        ( { modelAcc | level = Level.breakWall modelAcc.level sector Point3d.origin Vector3d.zero }, cmdAcc )

                    EnableUpsideDownWalking ->
                        ( { modelAcc | player = Player.enableUpsideDownWalking modelAcc.player }, cmdAcc )

                    ComeBackDown ->
                        ( { modelAcc | player = Player.comeBackDown modelAcc.player }, cmdAcc )
            )
            ( model, Cmd.none )


view : SceneAssets.Model -> Model -> Html Msg
view sceneAssets model =
    case model.state of
        FadingOutLevel timeLeft ->
            viewGame sceneAssets model (timeLeft / levelFadeOutTime)

        FadingInLevel timeLeft ->
            viewGame sceneAssets model ((levelFadeInTime - timeLeft) / levelFadeInTime)

        Playing ->
            viewGame sceneAssets model 1

        Initializing ->
            viewGame sceneAssets model 0

        FinalFadeOut timeLeft ->
            viewGame sceneAssets model (timeLeft / levelFadeOutTime)

        GameEnding ending ->
            Ending.view ending model.canvasSize


viewGame sceneAssets model opacity =
    Html.div [ Html.Attributes.style "background" model.fadeColor ]
        [ Html.div
            [ Html.Attributes.style "opacity" (String.fromFloat opacity)
            , Html.Attributes.style "visibility"
                (if model.state == Initializing then
                    "hidden"

                 else
                    "visible"
                )
            ]
            [ Scene3d.cloudy
                { entities =
                    [ Level.view sceneAssets (Player.getSector model.player) model.level
                    ]
                , camera = Player.view model.player
                , upDirection = Direction3d.z
                , background = Scene3d.backgroundColor model.backgroundColor
                , clipDepth = Length.centimeters 1
                , dimensions = Tuple.mapBoth Pixels.int Pixels.int model.canvasSize
                , visibility = model.visibility
                }
            ]
        , Narration.view model.narration
        ]
