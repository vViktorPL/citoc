module Player exposing
    ( Player
    , initOnLevel
    , view
    , turnLeft
    , turnRight
    , stopTurning
    , walkForward
    , walkBackward
    , standStill
    , update
    , teleport
    , getSector
    , getHorizontalOrientation
    )

import Point3d exposing (Point3d)
import Length
import Level exposing (Orientation(..), WorldCoordinates, pointOnLevel)
import Angle exposing (Angle)
import Camera3d
import Direction3d
import Viewpoint3d

type Player =
    Player
        { position: Point3d Length.Meters WorldCoordinates
        , horizontalAngle: Angle
        , horizontalTurning: HorizontalTurning
        , movement: Movement
        }

type HorizontalTurning
    = None
    | TurnLeft
    | TurnRight

type Movement
    = Stand
    | Forward
    | Backward

turningSpeed = 0.1
walkingSpeed = 0.002

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
        , horizontalTurning = None
        , movement = Stand
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
                direction = Direction3d.yx playerData.horizontalAngle
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

stopTurning : Player -> Player
stopTurning (Player playerData) =
    Player { playerData | horizontalTurning = None }

walkForward : Player -> Player
walkForward (Player playerData) =
    Player { playerData | movement = Forward }

walkBackward : Player -> Player
walkBackward (Player playerData) =
    Player { playerData | movement = Backward }

standStill : Player -> Player
standStill (Player playerData) =
    Player { playerData | movement = Stand }

update : Float -> Player -> Player
update delta player =
    player
        |> animatePlayerTurning delta
        |> animatePlayerMovement delta

animatePlayerMovement : Float -> Player -> Player
animatePlayerMovement delta (Player playerData) =
    let
       direction = Direction3d.yx playerData.horizontalAngle
       velocity = delta * walkingSpeed
    in
    case playerData.movement of
        Forward -> Player { playerData | position = Point3d.translateIn direction (Length.meters velocity) playerData.position }
        Backward -> Player { playerData | position = Point3d.translateIn direction (Length.meters -velocity) playerData.position }
        Stand -> Player playerData

animatePlayerTurning : Float -> Player -> Player
animatePlayerTurning delta (Player playerData) =
    case playerData.horizontalTurning of
        None -> (Player playerData)
        TurnLeft -> (Player { playerData | horizontalAngle = Angle.normalize (Angle.degrees ((Angle.inDegrees playerData.horizontalAngle) - delta * turningSpeed)) })
        TurnRight -> (Player { playerData | horizontalAngle = Angle.normalize (Angle.degrees ((Angle.inDegrees playerData.horizontalAngle) + delta * turningSpeed)) })