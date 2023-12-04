module Game exposing (..)

import Angle
import Assets
import Browser.Dom
import Browser.Events
import Color exposing (..)
import Coordinates
import Dict exposing (Dict)
import Direction3d
import Ending
import Html exposing (Html)
import Html.Attributes
import Json.Decode as Decode
import Length
import Level
import Level.Index as LevelIndex
import LevelTile
import Narration
import Orientation exposing (Orientation(..))
import Pixels
import Player exposing (Player)
import Point3d
import Scene3d
import Sound
import Task
import Trigger exposing (TriggerCondition, TriggerEffect)
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
        , Assets.subscription |> Sub.map AssetsMsg
        ]


type GameState
    = LoadingLevel
    | Playing
    | FadingOutLevel Float
    | FadingInLevel Float
    | FinalFadeOut Float
    | GameEnding Ending.Model


type alias Model =
    { state : GameState
    , player : Player
    , level : Level.Model
    , levelsLeft : List ( Level.Model, List Assets.Dependency )
    , counters : Dict String Int
    , gestureHistory : List Gesture
    , canvasSize : ( Int, Int )
    , fadeColor : String
    , backgroundColor : Color
    , visibility : Scene3d.Visibility
    , narration : Narration.Model
    , assets : Assets.Model
    , levelJustLoaded : Bool
    , windowShaken : Bool
    }


type Gesture
    = LookLeft
    | LookRight
    | LookUp
    | LookDown


maxGestureHistory =
    5


loadNextLevel : Model -> ( Model, Cmd Msg )
loadNextLevel model =
    let
        ( nextLevel, dependencies ) =
            model.levelsLeft
                |> List.head
                |> Maybe.withDefault ( model.level, [] )

        ( updatedAssets, assetsCmd ) =
            Assets.requestDependencies dependencies model.assets
    in
    ( { model
        | state = LoadingLevel
        , level = nextLevel
        , levelJustLoaded = True
        , levelsLeft = List.drop 1 model.levelsLeft
        , assets = updatedAssets
        , player = Level.initPlayer nextLevel
      }
    , Cmd.map AssetsMsg assetsCmd
    )


init : Assets.Model -> ( Model, Cmd Msg )
init assets =
    let
        ( level, dependencies ) =
            LevelIndex.firstLevel

        ( updatedAssets, assetsCmd ) =
            Assets.requestDependencies dependencies assets
    in
    ( { state = LoadingLevel
      , player = Level.initPlayer level
      , level = level
      , levelJustLoaded = True
      , levelsLeft = LevelIndex.restLevels
      , counters = Dict.empty
      , gestureHistory = []
      , canvasSize = ( 800, 600 )
      , fadeColor = "black"
      , backgroundColor = Color.white
      , visibility = Scene3d.clearView
      , narration = Narration.init
      , assets = updatedAssets
      , windowShaken = False
      }
    , Cmd.batch
        [ Task.perform
            (\viewportDetails -> WindowResize (floor viewportDetails.viewport.width) (floor viewportDetails.viewport.height))
            Browser.Dom.getViewport
        , Sound.stopMusic ()
        , Cmd.map AssetsMsg assetsCmd
        ]
    )


type Msg
    = AnimationTick Float
    | AssetsMsg Assets.Msg
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


playerCollides : Level.Model -> Player -> Bool
playerCollides level player =
    case Level.interact level player Vector3d.zero of
        ( Level.LevelCollision _, _ ) ->
            True

        _ ->
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


handleStepSounds : Level.Model -> Player.Player -> Player.OutMsg -> Cmd msg
handleStepSounds level player outmsg =
    case outmsg of
        Player.EmitStepSound soundNumber ->
            case Level.getGroundSound level (Player.getSector player) of
                LevelTile.SolidFloor ->
                    Sound.playSound ("step-" ++ String.fromInt soundNumber ++ ".mp3")

                LevelTile.SandGround ->
                    Sound.playSound "sand-step.mp3"

                _ ->
                    Cmd.none

        _ ->
            Cmd.none


updateAnimation : Float -> Model -> ( Model, Cmd Msg )
updateAnimation delta model =
    case model.state of
        LoadingLevel ->
            ( model, Cmd.none )

        --( { model | state = FadingInLevel initFadeInTime }, Cmd.none )
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
                    Level.interact modelToUpdate.level model.player v
            in
            case interactionResult of
                Level.LevelUpdated updatedLevel ->
                    ( { modelToUpdate | level = updatedLevel }, interactionCmd )

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
            in
            if newTimeLeft == 0 then
                loadNextLevel model

            else
                ( { model | state = FadingOutLevel newTimeLeft, counters = Dict.empty }, Cmd.none )

        FinalFadeOut timeLeft ->
            let
                newTimeLeft =
                    max (timeLeft - delta) 0
            in
            if timeLeft == 0 then
                ( { model | state = GameEnding (Ending.init model.assets) }, Cmd.none )

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

        AssetsMsg assetsMsg ->
            let
                ( updatedAssets, assetsCmd ) =
                    Assets.update assetsMsg model.assets
            in
            ( { model
                | assets = updatedAssets
                , state =
                    case ( model.state, Assets.areReady updatedAssets ) of
                        ( LoadingLevel, True ) ->
                            FadingInLevel initFadeInTime

                        _ ->
                            model.state
              }
            , Cmd.map AssetsMsg assetsCmd
            )

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
                    ( { model | windowShaken = True }, Cmd.none )

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
                            Trigger.LevelLoaded ->
                                model.levelJustLoaded

                            Trigger.InSector sector ->
                                sector == ( newX, newY )

                            Trigger.EnteredFrom orientation ->
                                case orientation of
                                    North ->
                                        dY < 0

                                    South ->
                                        dY > 0

                                    East ->
                                        dX > 0

                                    West ->
                                        dX < 0

                            Trigger.LookAngle orientation ->
                                orientation == lookingAt

                            Trigger.LookingAtGround ->
                                Angle.inDegrees (Player.getVerticalLookAngle newPlayer) <= -45

                            Trigger.NegativeHeadshake ->
                                let
                                    last3Gestures =
                                        List.take 3 model.gestureHistory
                                in
                                last3Gestures
                                    == [ LookLeft, LookRight, LookLeft ]
                                    || last3Gestures
                                    == [ LookRight, LookLeft, LookRight ]

                            Trigger.Nod ->
                                let
                                    last3Gestures =
                                        List.take 3 model.gestureHistory
                                in
                                last3Gestures
                                    == [ LookUp, LookDown, LookUp ]
                                    || last3Gestures
                                    == [ LookDown, LookUp, LookDown ]

                            Trigger.SteppedIn ->
                                dX /= 0 || dY /= 0

                            Trigger.CameBackToFloor ->
                                Player.isUpsideDown model.player && not (Player.isUpsideDown newPlayer)

                            Trigger.CounterEquals counterName desiredNumber ->
                                let
                                    counterState =
                                        Maybe.withDefault 0 (Dict.get counterName model.counters)
                                in
                                desiredNumber == counterState

                            Trigger.WindowShake ->
                                model.windowShaken
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
                , windowShaken = False
                , levelJustLoaded = False
            }


executeEffects : Model -> List TriggerEffect -> ( Model, Cmd Msg )
executeEffects model effects =
    effects
        |> List.foldl
            (\effect ( modelAcc, cmdAcc ) ->
                case effect of
                    Trigger.Teleport targetSector ->
                        ( { modelAcc | player = Player.seamlessTeleport modelAcc.player targetSector }, cmdAcc )

                    Trigger.NextLevel ->
                        ( transitionToNextLevel modelAcc, Cmd.batch [ cmdAcc, Sound.playSound "success.mp3" ] )

                    Trigger.ChangeTile sector newTile ->
                        ( { modelAcc | level = Level.updateTile sector newTile modelAcc.level }, cmdAcc )

                    Trigger.CreateTrigger trigger ->
                        ( { modelAcc | level = Level.addTrigger trigger modelAcc.level }, cmdAcc )

                    Trigger.RemoveAllTriggersInSector sector ->
                        ( { modelAcc | level = Level.removeAllTriggersAtSector sector modelAcc.level }, cmdAcc )

                    Trigger.RemoveAllTriggersInSectors sectors ->
                        ( { modelAcc | level = Level.removeAllTriggersAtSectors sectors modelAcc.level }, cmdAcc )

                    Trigger.IncrementCounter counterName ->
                        ( { modelAcc | counters = Dict.update counterName (\prevCount -> Just (Maybe.withDefault 0 prevCount + 1)) modelAcc.counters }
                        , Cmd.batch [ cmdAcc, Sound.playSound "notify-up.mp3" ]
                        )

                    Trigger.DecrementCounter counterName ->
                        ( { modelAcc | counters = Dict.update counterName (\prevCount -> Just (Maybe.withDefault 0 prevCount - 1)) modelAcc.counters }
                        , Cmd.batch [ cmdAcc, Sound.playSound "notify-down.mp3" ]
                        )

                    Trigger.PlaySound fileName ->
                        ( modelAcc, Cmd.batch [ cmdAcc, Sound.playSound fileName ] )

                    Trigger.InitFog color distance ->
                        ( { modelAcc | backgroundColor = color, visibility = Scene3d.fog distance }, cmdAcc )

                    Trigger.SitDown ->
                        ( { modelAcc | player = Player.sitDown modelAcc.player }, cmdAcc )

                    Trigger.OpenTerms sector ->
                        ( { modelAcc | level = Level.activateTile modelAcc.level sector }, Cmd.batch [ cmdAcc, Sound.playSound "elevator_door.mp3" ] )

                    Trigger.PlayMusic fileName ->
                        ( modelAcc, Cmd.batch [ cmdAcc, Sound.playMusic fileName ] )

                    Trigger.StartNarration narrationNumber ->
                        let
                            ( narration, narrationCmd ) =
                                Narration.playNarration modelAcc.narration narrationNumber
                        in
                        ( { modelAcc | narration = narration }, Cmd.batch [ cmdAcc, narrationCmd ] )

                    Trigger.ShowGameEndingScreen ->
                        ( { modelAcc | state = FinalFadeOut levelFadeOutTime, fadeColor = "black" }, Cmd.batch [ cmdAcc, Sound.stopMusic () ] )

                    Trigger.BreakWall sector ->
                        ( { modelAcc | level = Level.breakWall modelAcc.level sector Point3d.origin Vector3d.zero }, cmdAcc )

                    Trigger.EnableUpsideDownWalking ->
                        ( { modelAcc | player = Player.enableUpsideDownWalking modelAcc.player }, cmdAcc )

                    Trigger.ComeBackDown ->
                        ( { modelAcc | player = Player.comeBackDown modelAcc.player }, cmdAcc )
            )
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    case model.state of
        FadingOutLevel timeLeft ->
            viewGame model (timeLeft / levelFadeOutTime)

        FadingInLevel timeLeft ->
            viewGame model ((levelFadeInTime - timeLeft) / levelFadeInTime)

        Playing ->
            viewGame model 1

        LoadingLevel ->
            viewGame model 0

        FinalFadeOut timeLeft ->
            viewGame model (timeLeft / levelFadeOutTime)

        GameEnding ending ->
            Ending.view ending model.canvasSize


viewGame model opacity =
    let
        playerSector =
            model.player
                |> Player.getPlayerPosition
                |> Coordinates.worldPositionToSector
    in
    Html.div [ Html.Attributes.style "background" model.fadeColor ]
        [ Html.div
            [ Html.Attributes.style "opacity" (String.fromFloat opacity)
            , Html.Attributes.style "visibility"
                (if model.state == LoadingLevel then
                    "hidden"

                 else
                    "visible"
                )
            ]
            [ Scene3d.cloudy
                { entities =
                    [ Level.view model.assets model.level playerSector
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
        , Html.div
            [ Html.Attributes.class "loadingScreen"
            , Html.Attributes.style
                "visibility"
                (if model.state == LoadingLevel then
                    "visible"

                 else
                    "hidden"
                )
            ]
            [ Html.div [ Html.Attributes.class "loadingSpinner" ] []
            , Html.text "Loading..."
            ]

        --, Html.div
        --    [ Html.Attributes.style "visibility"
        --        (if model.state == LoadingLevel then
        --            "hidden"
        --
        --         else
        --            "visible"
        --        )
        --    ]
        --    [ Html.text "Loading..." ]
        ]
