module Player exposing
    ( Player
    , initOnLevel
    , view
    , turnLeft
    , turnRight
    , stopTurning
    , walkForward
    , walkBackward
    , strafeLeft
    , strafeRight
    , updateLookByMouseMovement
    , standStill
    , stopStrafingLeft
    , stopStrafingRight
    , stopWalkingBackward
    , stopWalkingForward
    , update
    , teleport
    , getSector
    , getHorizontalOrientation
    , getMovementVector
    , getPlayerPosition
    , updatePlayerPosition
    , playerRadius
    )

import Point3d exposing (Point3d)
import Length
import Level exposing ( WorldCoordinates, pointOnLevel)
import Orientation exposing (Orientation(..))
import Angle exposing (Angle)
import Camera3d
import Direction3d
import Viewpoint3d
import Vector3d exposing (Vector3d)
import Sound

type Player =
    Player
        { position: Point3d Length.Meters WorldCoordinates
        , horizontalAngle: Angle
        , verticalAngle: Angle
        , horizontalTurning: HorizontalTurning
        , movementX: Maybe MovementX
        , movementY: Maybe MovementY
        , remainingTimeToStepSound: Maybe Float
        , lastStepSoundNumber: Int
        }

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

turningSpeed = 0.1
walkingSpeed = 0.002
playerRadius = Length.centimeters 20

init : (Int, Int) -> Orientation -> Player
init (x, y) orientation =
    Player
        { position = pointOnLevel ((toFloat x) + 0.5) ((toFloat y) + 0.5) 0.5
        , horizontalAngle =
            case orientation of
                North -> Angle.degrees 180
                East -> Angle.degrees -90
                South -> Angle.degrees 0
                West -> Angle.degrees 90
        , verticalAngle = Angle.degrees 0
        , horizontalTurning = None
        , movementX = Nothing
        , movementY = Nothing
        , remainingTimeToStepSound = Nothing
        , lastStepSoundNumber = 1
        }

teleport : Player -> (Int, Int) -> Player
teleport (Player playerData) (x, y) =
    let
        oldPosition3d = Point3d.toMeters playerData.position
        offsetX = -oldPosition3d.x - toFloat (floor -oldPosition3d.x)
        offsetY = oldPosition3d.y - toFloat (floor oldPosition3d.y)
    in
    Player
        { playerData | position = pointOnLevel ((toFloat x) + offsetX) ((toFloat y) + offsetY) 0.5
        }

getHorizontalOrientation : Player -> Orientation
getHorizontalOrientation (Player playerData) =
    let
        deg = (Angle.inDegrees playerData.horizontalAngle)
    in
        (if deg >= 145 || deg <= -145 then
            North
        else if deg > -145 && deg <= -55 then
            East
        else if deg > -55 && deg <= 35 then
            South
        else
            West
        )

getSector : Player -> (Int, Int)
getSector (Player playerData) =
    let
        { x, y } = Point3d.toMeters playerData.position
    in
    (floor -x, floor y)

initOnLevel : Level.Level -> Player
initOnLevel level =
    let
        startingPosition = Level.getStartingPosition level
        startingOrientation = Level.getStartingOrientation level
    in
        init startingPosition startingOrientation

view : Player -> Camera3d.Camera3d Length.Meters WorldCoordinates
view (Player playerData) =
     Camera3d.perspective
          { viewpoint =
            let
                horizontalAngle = Angle.degrees (90 - (Angle.inDegrees playerData.horizontalAngle))
                direction = Direction3d.xyZ horizontalAngle playerData.verticalAngle
            in
                Viewpoint3d.lookAt
                    { eyePoint = playerData.position
                    , focalPoint = Point3d.translateIn direction Length.meter playerData.position
                    , upDirection = Direction3d.positiveZ
                    }
          , verticalFieldOfView = Angle.degrees 45
          }

turnLeft : Player -> Player
turnLeft (Player playerData) =
    Player { playerData | horizontalTurning = TurnLeft }

turnRight : Player -> Player
turnRight (Player playerData) =
    Player { playerData | horizontalTurning = TurnRight }

mouseSensitivity = 0.25

updateLookByMouseMovement : (Int, Int) -> Player -> Player
updateLookByMouseMovement (dx, dy) (Player playerData) =
    (Player { playerData
        | horizontalAngle = Angle.normalize (Angle.degrees ((Angle.inDegrees playerData.horizontalAngle) + toFloat dx * mouseSensitivity))
        , verticalAngle = Angle.degrees (clamp -89.9 89.9 ((Angle.inDegrees playerData.verticalAngle) - toFloat dy * mouseSensitivity * 0.5))
    })

stopTurning : Player -> Player
stopTurning (Player playerData) =
    Player { playerData | horizontalTurning = None }

walkForward : Player -> Player
walkForward (Player playerData) =
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
        Just StrafeLeft -> Player { playerData | movementX = Nothing }
        _ -> Player playerData


stopStrafingRight : Player -> Player
stopStrafingRight (Player playerData) =
    case playerData.movementX of
        Just StrafeRight -> Player { playerData | movementX = Nothing }
        _ -> Player playerData

stopWalkingForward : Player -> Player
stopWalkingForward (Player playerData) =
    case playerData.movementY of
        Just Forward -> Player { playerData | movementY = Nothing }
        _ -> Player playerData

stopWalkingBackward : Player -> Player
stopWalkingBackward (Player playerData) =
    case playerData.movementY of
        Just Backward -> Player { playerData | movementY = Nothing }
        _ -> Player playerData


standStill : Player -> Player
standStill (Player playerData) =
    Player { playerData | movementX = Nothing, movementY = Nothing, remainingTimeToStepSound = Nothing }

update : Float -> Player -> (Player, Cmd msg)
update delta player =
    player
        |> animatePlayerTurning delta
        |> updatePlayerSteps delta
        --|> animatePlayerMovement delta

updatePlayerSteps : Float -> Player -> (Player, Cmd msg)
updatePlayerSteps delta (Player playerData) =
    case playerData.remainingTimeToStepSound of
        Just previousTime ->
            let
                newTime = previousTime - delta
                newStepSoundNumber = calculateStepSoundNumber delta playerData.lastStepSoundNumber
            in
                if newTime <= 0 then
                    ( Player
                        { playerData
                        | remainingTimeToStepSound = Just millisecondsPerStep
                        , lastStepSoundNumber = newStepSoundNumber
                        }
                    , Sound.playSound ("step-" ++ String.fromInt newStepSoundNumber  ++ ".mp3")
                    )
                else
                    (Player { playerData | remainingTimeToStepSound = Just newTime }, Cmd.none)
        Nothing -> (Player playerData, Cmd.none)

calculateStepSoundNumber : Float -> Int -> Int
calculateStepSoundNumber seed lastStepSoundNumber =
    let
        stepSoundNumber = modBy 5 (floor seed) + 1
    in
        if stepSoundNumber == lastStepSoundNumber then
            (modBy 5 lastStepSoundNumber) + 1
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
    case playerData.horizontalTurning of
        None -> (Player playerData)
        TurnLeft -> (Player { playerData | horizontalAngle = Angle.normalize (Angle.degrees ((Angle.inDegrees playerData.horizontalAngle) - delta * turningSpeed)) })
        TurnRight -> (Player { playerData | horizontalAngle = Angle.normalize (Angle.degrees ((Angle.inDegrees playerData.horizontalAngle) + delta * turningSpeed)) })

animatePlayerMovement : Float -> Player -> Player
animatePlayerMovement delta player =
    let
        v = getMovementVector player
            |> Vector3d.scaleBy delta
    in
        updatePlayerPosition v player

millisecondsPerStep = 500

updatePlayerPosition : Vector3d Length.Meters WorldCoordinates -> Player -> Player
updatePlayerPosition v (Player playerData) =
    Player { playerData | position =
        (Point3d.toMeters playerData.position)
            |> Vector3d.fromMeters
            |> Vector3d.plus v
            |> Vector3d.toMeters
            |> Point3d.fromMeters
        , remainingTimeToStepSound =
            if ((v |> Vector3d.length |> Length.inMeters |> abs) > 0) then
                case playerData.remainingTimeToStepSound of
                    Just time -> Just time
                    Nothing -> Just millisecondsPerStep
            else
                Nothing
    }

getPlayerPosition : Player -> Point3d Length.Meters WorldCoordinates
getPlayerPosition (Player playerData) =
    playerData.position

getMovementVector : Player -> Vector3d Length.Meters WorldCoordinates
getMovementVector (Player playerData) =
    let
        forwardAngle = playerData.horizontalAngle
        strafeLeftAngle = (addAngle playerData.horizontalAngle -90)

        forwardVector = Vector3d.fromMeters
            { x = Angle.sin forwardAngle
            , y = Angle.cos forwardAngle
            , z = 0
            }
        strafeLeftVector = Vector3d.fromMeters
            { x = Angle.sin strafeLeftAngle
            , y = Angle.cos strafeLeftAngle
            , z = 0
            }

        forwardSpeed = case playerData.movementY of
            Just Forward -> walkingSpeed
            Just Backward -> -walkingSpeed
            _ -> 0
        strafeSpeed = case playerData.movementX of
            Just StrafeLeft -> walkingSpeed * 0.7
            Just StrafeRight -> -walkingSpeed * 0.7
            _ -> 0
    in
        forwardVector
            |> Vector3d.scaleBy forwardSpeed
            |> Vector3d.plus (Vector3d.scaleBy strafeSpeed strafeLeftVector)


