module Player exposing
    ( OutMsg(..)
    , Player
    , changeMouseSensitivity
    , comeBackDown
    , enableUpsideDownWalking
    , getHorizontalOrientation
    , getMovementVector
    , getPlayerPosition
    , getSector
    , getVerticalLookAngle
    , init
    , isUpsideDown
    , orientedToSector
    , playerRadius
    , safeTeleport
    , seamlessTeleport
    , sitDown
    , standStill
    , stopStrafingLeft
    , stopStrafingRight
    , stopTurning
    , stopWalkingBackward
    , stopWalkingForward
    , strafeLeft
    , strafeRight
    , turnLeft
    , turnRight
    , update
    , updateLookByMouseMovement
    , updatePlayerPosition
    , view
    , walkBackward
    , walkForward
    )

import Angle exposing (Angle)
import Camera3d
import Coordinates exposing (ObjectCoordinates, SectorCoordinates, WorldCoordinates)
import Direction3d
import Length exposing (Length)
import Orientation exposing (Orientation(..))
import Point3d exposing (Point3d)
import Vector3d exposing (Vector3d)
import Viewpoint3d


type Player
    = Player PlayerData


type alias PlayerData =
    { position : Point3d Length.Meters WorldCoordinates
    , horizontalAngle : Angle
    , verticalAngle : Angle
    , horizontalTurning : HorizontalTurning
    , movementX : Maybe MovementX
    , movementY : Maybe MovementY
    , remainingTimeToStepSound : Maybe Float
    , lastStepSoundNumber : Int
    , will : PlayerWill
    , upsideDownMode : UpsideDownMode
    , mouseSensitivity : Float
    }


type UpsideDownMode
    = WalkOnFloor
    | EnableUpsideDown
    | EnableOnFloorWalking
    | WalkOnCeiling


type OutMsg
    = Noop
    | EmitStepSound Int


type PlayerWill
    = FreeWill
    | SittingDown
        { targetHorizontalAngle : Angle
        , initialHorizontalAngle : Angle
        , zPositionOffset : Length
        , completed : Bool
        }
    | StandingUp Length
    | ComingBackDown


type HorizontalTurning
    = None
    | TurnLeft
    | TurnRight


type MovementY
    = Forward
    | Backward


type MovementX
    = StrafeLeft
    | StrafeRight


turningSpeed =
    0.1


walkingSpeed =
    0.002


playerRadius =
    Length.centimeters 20


playerHeight =
    Length.meters 0.5


playerHeightVector =
    Vector3d.xyz (Length.meters 0) (Length.meters 0) playerHeight


changeMouseSensitivity : Float -> Player -> Player
changeMouseSensitivity newMouseSensitivity (Player model) =
    Player { model | mouseSensitivity = newMouseSensitivity }


init : Float -> SectorCoordinates -> Orientation -> Player
init mouseSensitivity sector orientation =
    Player
        { position =
            Coordinates.sectorToWorldPosition sector
                |> Point3d.translateBy playerHeightVector
        , horizontalAngle =
            case orientation of
                North ->
                    Angle.degrees 180

                East ->
                    Angle.degrees -90

                South ->
                    Angle.degrees 0

                West ->
                    Angle.degrees 90
        , verticalAngle = Angle.degrees 0
        , horizontalTurning = None
        , movementX = Nothing
        , movementY = Nothing
        , remainingTimeToStepSound = Nothing
        , lastStepSoundNumber = 1
        , will = FreeWill
        , upsideDownMode = WalkOnFloor
        , mouseSensitivity = mouseSensitivity
        }


seamlessTeleport : Player -> SectorCoordinates -> Player
seamlessTeleport (Player playerData) ( x, y ) =
    let
        oldPosition3d =
            Point3d.toMeters playerData.position

        offsetX =
            -oldPosition3d.x - toFloat (floor -oldPosition3d.x)

        offsetY =
            oldPosition3d.y - toFloat (floor oldPosition3d.y)
    in
    Player
        { playerData
            | position = Point3d.meters -(toFloat x + offsetX) (toFloat y + offsetY) oldPosition3d.z
        }


safeTeleport : Player -> SectorCoordinates -> Player
safeTeleport (Player playerData) ( x, y ) =
    Player
        { playerData
            | position = Point3d.meters -(toFloat x + 0.5) (toFloat y + 0.5) (Point3d.toMeters playerData.position).z
        }


isUpsideDown : Player -> Bool
isUpsideDown (Player playerData) =
    abs (Angle.inDegrees playerData.verticalAngle) >= 90


getHorizontalOrientation : Player -> Orientation
getHorizontalOrientation (Player playerData) =
    let
        deg =
            Angle.inDegrees playerData.horizontalAngle
    in
    if deg >= 145 || deg <= -145 then
        North

    else if deg > -145 && deg <= -55 then
        East

    else if deg > -55 && deg <= 35 then
        South

    else
        West


getVerticalLookAngle : Player -> Angle
getVerticalLookAngle (Player playerData) =
    playerData.verticalAngle


getSector : Player -> SectorCoordinates
getSector (Player playerData) =
    let
        { x, y } =
            Point3d.toMeters playerData.position
    in
    ( floor -x, floor y )


orientedToSector : Player -> SectorCoordinates
orientedToSector player =
    let
        orientation =
            getHorizontalOrientation player

        ( x, y ) =
            getSector player
    in
    case orientation of
        North ->
            ( x, y - 1 )

        East ->
            ( x + 1, y )

        South ->
            ( x, y + 1 )

        West ->
            ( x - 1, y )


view : Player -> Camera3d.Camera3d Length.Meters WorldCoordinates
view (Player playerData) =
    Camera3d.perspective
        { viewpoint =
            let
                upsideDown =
                    isUpsideDown (Player playerData)

                horizontalAngle =
                    Angle.degrees (90 - Angle.inDegrees playerData.horizontalAngle)

                direction =
                    Direction3d.xyZ horizontalAngle playerData.verticalAngle

                playerPos =
                    getPlayerPosition (Player playerData)
            in
            Viewpoint3d.lookAt
                { eyePoint = playerPos
                , focalPoint = Point3d.translateIn direction Length.meter playerPos
                , upDirection =
                    if upsideDown then
                        Direction3d.negativeZ

                    else
                        Direction3d.positiveZ
                }
        , verticalFieldOfView = Angle.degrees 45
        }


turnLeft : Player -> Player
turnLeft (Player playerData) =
    Player { playerData | horizontalTurning = TurnLeft }


turnRight : Player -> Player
turnRight (Player playerData) =
    Player { playerData | horizontalTurning = TurnRight }



--mouseSensitivity =
--    0.25


updateLookByMouseMovement : ( Int, Int ) -> Player -> Player
updateLookByMouseMovement ( dx, dy ) (Player playerData) =
    case playerData.will of
        FreeWill ->
            let
                horizontalAngleDelta =
                    toFloat dx
                        * playerData.mouseSensitivity
                        * (if isUpsideDown (Player playerData) then
                            -1

                           else
                            1
                          )

                newPlayerData =
                    { playerData
                        | horizontalAngle = Angle.normalize (Angle.degrees (Angle.inDegrees playerData.horizontalAngle + horizontalAngleDelta))
                        , verticalAngle =
                            playerData.verticalAngle
                                |> Angle.inDegrees
                                |> (\deg -> deg - toFloat dy * playerData.mouseSensitivity * 0.5)
                                |> Angle.degrees
                                |> limitVerticalAngle playerData
                    }

                wasUpsideDownBefore =
                    isUpsideDown (Player playerData)

                isUpsideDownAfter =
                    isUpsideDown (Player newPlayerData)
            in
            case ( newPlayerData.upsideDownMode, wasUpsideDownBefore, isUpsideDownAfter ) of
                ( EnableUpsideDown, False, True ) ->
                    Player
                        { newPlayerData
                            | upsideDownMode = WalkOnCeiling
                        }

                ( EnableOnFloorWalking, True, False ) ->
                    Player
                        { newPlayerData
                            | upsideDownMode = WalkOnFloor
                        }

                _ ->
                    Player newPlayerData

        _ ->
            Player playerData


limitVerticalAngle : PlayerData -> Angle -> Angle
limitVerticalAngle { upsideDownMode } newAngle =
    case upsideDownMode of
        WalkOnFloor ->
            newAngle
                |> Angle.inDegrees
                |> clamp -89.9 89.9
                |> Angle.degrees

        EnableUpsideDown ->
            newAngle
                |> Angle.inDegrees
                |> clamp -89.9 269.9
                |> Angle.degrees

        EnableOnFloorWalking ->
            newAngle
                |> Angle.inDegrees
                |> clamp -269.9 269.9
                |> Angle.degrees

        WalkOnCeiling ->
            newAngle
                |> Angle.inDegrees
                |> clamp 90.1 269.9
                |> Angle.degrees


stopTurning : Player -> Player
stopTurning (Player playerData) =
    Player { playerData | horizontalTurning = None }


walkForward : Player -> Player
walkForward (Player playerData) =
    case playerData.will of
        SittingDown _ ->
            tryToStandUp (Player playerData)

        _ ->
            Player { playerData | movementY = Just Forward }


walkBackward : Player -> Player
walkBackward (Player playerData) =
    Player { playerData | movementY = Just Backward }


strafeLeft : Player -> Player
strafeLeft (Player playerData) =
    Player { playerData | movementX = Just StrafeLeft }


strafeRight : Player -> Player
strafeRight (Player playerData) =
    Player { playerData | movementX = Just StrafeRight }


stopStrafingLeft : Player -> Player
stopStrafingLeft (Player playerData) =
    case playerData.movementX of
        Just StrafeLeft ->
            Player { playerData | movementX = Nothing }

        _ ->
            Player playerData


stopStrafingRight : Player -> Player
stopStrafingRight (Player playerData) =
    case playerData.movementX of
        Just StrafeRight ->
            Player { playerData | movementX = Nothing }

        _ ->
            Player playerData


stopWalkingForward : Player -> Player
stopWalkingForward (Player playerData) =
    case playerData.movementY of
        Just Forward ->
            Player { playerData | movementY = Nothing }

        _ ->
            Player playerData


stopWalkingBackward : Player -> Player
stopWalkingBackward (Player playerData) =
    case playerData.movementY of
        Just Backward ->
            Player { playerData | movementY = Nothing }

        _ ->
            Player playerData


standStill : Player -> Player
standStill (Player playerData) =
    Player { playerData | movementX = Nothing, movementY = Nothing, remainingTimeToStepSound = Nothing }


update : Float -> Player -> ( Player, OutMsg )
update delta player =
    player
        |> animatePlayerTurning delta
        |> animatePlayer delta
        |> updatePlayerSteps delta


animatePlayer : Float -> Player -> Player
animatePlayer delta (Player playerData) =
    case playerData.will of
        FreeWill ->
            Player playerData

        SittingDown { initialHorizontalAngle, targetHorizontalAngle, zPositionOffset } ->
            let
                playerPrevHorizontalAngleDeg =
                    Angle.inDegrees playerData.horizontalAngle

                playerInitialHorizontalAngleDeg =
                    Angle.inDegrees initialHorizontalAngle

                playerTargetHorizontalAngleDeg =
                    Angle.inDegrees targetHorizontalAngle

                angleStep =
                    delta * 0.001 * (playerTargetHorizontalAngleDeg - playerInitialHorizontalAngleDeg)

                playerNewHorizontalAngleDeg =
                    if playerTargetHorizontalAngleDeg > playerInitialHorizontalAngleDeg then
                        min playerTargetHorizontalAngleDeg (playerPrevHorizontalAngleDeg + angleStep)

                    else
                        max playerTargetHorizontalAngleDeg (playerPrevHorizontalAngleDeg + angleStep)

                newZPositionOffsetFloat =
                    max (Length.inMeters zPositionOffset - delta * 0.0001) -0.2

                newZPositionOffset =
                    Length.meters newZPositionOffsetFloat
            in
            Player
                { playerData
                    | horizontalAngle = Angle.degrees playerNewHorizontalAngleDeg
                    , will = SittingDown { initialHorizontalAngle = initialHorizontalAngle, targetHorizontalAngle = targetHorizontalAngle, zPositionOffset = newZPositionOffset, completed = newZPositionOffsetFloat == -0.2 }
                }

        StandingUp zPositionOffset ->
            let
                newZPositionOffsetFloat =
                    min (Length.inMeters zPositionOffset + delta * 0.0001) 0

                newZPositionOffset =
                    Length.meters newZPositionOffsetFloat
            in
            if newZPositionOffsetFloat == 0 then
                let
                    { x, y, z } =
                        Point3d.toMeters playerData.position

                    newY =
                        toFloat (floor y) - 0.01
                in
                Player { playerData | will = FreeWill, position = Point3d.unsafe { x = x, y = newY, z = z } }

            else
                Player { playerData | will = StandingUp newZPositionOffset }

        ComingBackDown ->
            let
                newPlayerVerticalAngleDeg =
                    Angle.inDegrees playerData.verticalAngle + delta * 0.3

                updatedPlayerData =
                    { playerData | verticalAngle = Angle.degrees newPlayerVerticalAngleDeg }
            in
            Player
                (if newPlayerVerticalAngleDeg < 270 then
                    updatedPlayerData

                 else
                    { updatedPlayerData
                        | verticalAngle = Angle.degrees -89.9
                        , upsideDownMode = WalkOnFloor
                        , will = FreeWill
                    }
                )


updatePlayerSteps : Float -> Player -> ( Player, OutMsg )
updatePlayerSteps delta (Player playerData) =
    case playerData.remainingTimeToStepSound of
        Just previousTime ->
            let
                newTime =
                    previousTime - delta

                newStepSoundNumber =
                    calculateStepSoundNumber delta playerData.lastStepSoundNumber
            in
            if newTime <= 0 then
                ( Player
                    { playerData
                        | remainingTimeToStepSound = Just millisecondsPerStep
                        , lastStepSoundNumber = newStepSoundNumber
                    }
                , EmitStepSound newStepSoundNumber
                )

            else
                ( Player { playerData | remainingTimeToStepSound = Just newTime }, Noop )

        Nothing ->
            ( Player playerData, Noop )


calculateStepSoundNumber : Float -> Int -> Int
calculateStepSoundNumber seed lastStepSoundNumber =
    let
        stepSoundNumber =
            modBy 5 (floor seed) + 1
    in
    if stepSoundNumber == lastStepSoundNumber then
        modBy 5 lastStepSoundNumber + 1

    else
        stepSoundNumber


addAngle : Angle -> Float -> Angle
addAngle angle degrees =
    angle
        |> Angle.inDegrees
        |> (+) degrees
        |> Angle.degrees


animatePlayerTurning : Float -> Player -> Player
animatePlayerTurning delta (Player playerData) =
    case ( playerData.horizontalTurning, playerData.will ) of
        ( TurnLeft, FreeWill ) ->
            Player { playerData | horizontalAngle = Angle.normalize (Angle.degrees (Angle.inDegrees playerData.horizontalAngle - delta * turningSpeed)) }

        ( TurnRight, FreeWill ) ->
            Player { playerData | horizontalAngle = Angle.normalize (Angle.degrees (Angle.inDegrees playerData.horizontalAngle + delta * turningSpeed)) }

        _ ->
            Player playerData


millisecondsPerStep =
    500


updatePlayerPosition : Vector3d Length.Meters WorldCoordinates -> Player -> Player
updatePlayerPosition v (Player playerData) =
    case playerData.will of
        FreeWill ->
            Player
                { playerData
                    | position =
                        Point3d.toMeters playerData.position
                            |> Vector3d.fromMeters
                            |> Vector3d.plus v
                            |> Vector3d.toMeters
                            |> Point3d.fromMeters
                    , remainingTimeToStepSound =
                        if (v |> Vector3d.length |> Length.inMeters |> abs) > 0 then
                            case playerData.remainingTimeToStepSound of
                                Just time ->
                                    Just time

                                Nothing ->
                                    Just millisecondsPerStep

                        else
                            Nothing
                }

        _ ->
            Player playerData


getPlayerPosition : Player -> Point3d Length.Meters WorldCoordinates
getPlayerPosition (Player playerData) =
    case playerData.will of
        SittingDown { zPositionOffset } ->
            let
                originalPos =
                    Point3d.toMeters playerData.position
            in
            Point3d.fromMeters
                { x = originalPos.x
                , y = originalPos.y - Length.inMeters zPositionOffset
                , z = originalPos.z + Length.inMeters zPositionOffset
                }

        StandingUp zPositionOffset ->
            let
                originalPos =
                    Point3d.toMeters playerData.position
            in
            Point3d.fromMeters
                { x = originalPos.x
                , y = originalPos.y - Length.inMeters zPositionOffset
                , z = originalPos.z + Length.inMeters zPositionOffset
                }

        _ ->
            playerData.position


getMovementVector : Player -> Vector3d Length.Meters WorldCoordinates
getMovementVector (Player playerData) =
    let
        upsideDown =
            isUpsideDown (Player playerData)

        forwardAngle =
            if upsideDown then
                addAngle playerData.horizontalAngle 180

            else
                playerData.horizontalAngle

        strafeLeftAngle =
            addAngle playerData.horizontalAngle -90

        forwardVector =
            Vector3d.fromMeters
                { x = Angle.sin forwardAngle
                , y = Angle.cos forwardAngle
                , z = 0
                }

        strafeLeftVector =
            Vector3d.fromMeters
                { x = Angle.sin strafeLeftAngle
                , y = Angle.cos strafeLeftAngle
                , z = 0
                }

        forwardSpeed =
            case playerData.movementY of
                Just Forward ->
                    walkingSpeed

                Just Backward ->
                    -walkingSpeed

                _ ->
                    0

        strafeSpeed =
            case playerData.movementX of
                Just StrafeLeft ->
                    walkingSpeed * 0.7

                Just StrafeRight ->
                    -walkingSpeed * 0.7

                _ ->
                    0
    in
    forwardVector
        |> Vector3d.scaleBy forwardSpeed
        |> Vector3d.plus (Vector3d.scaleBy strafeSpeed strafeLeftVector)


sitDown : Player -> Player
sitDown (Player playerData) =
    case playerData.will of
        SittingDown _ ->
            Player playerData

        _ ->
            Player
                { playerData
                    | will =
                        SittingDown
                            { targetHorizontalAngle = Angle.degrees 180
                            , initialHorizontalAngle = playerData.horizontalAngle
                            , zPositionOffset = Length.meters 0
                            , completed = False
                            }
                }
                |> standStill


tryToStandUp : Player -> Player
tryToStandUp (Player playerData) =
    case playerData.will of
        SittingDown { completed, zPositionOffset } ->
            if completed then
                Player { playerData | will = StandingUp zPositionOffset }

            else
                Player playerData

        _ ->
            Player playerData


enableUpsideDownWalking : Player -> Player
enableUpsideDownWalking (Player playerData) =
    case playerData.upsideDownMode of
        WalkOnFloor ->
            Player { playerData | upsideDownMode = EnableUpsideDown }

        _ ->
            Player playerData


comeBackDown : Player -> Player
comeBackDown (Player playerData) =
    case playerData.upsideDownMode of
        WalkOnCeiling ->
            Player { playerData | will = ComingBackDown }

        _ ->
            Player playerData
