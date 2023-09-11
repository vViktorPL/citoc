module Level exposing (..)

import Point3d
import Length
import Array exposing (Array)
import Scene3d
import Block3d
import Scene3d.Material
import Color
--import Player exposing (Player)

type Level
    = Level
        { tiles : Array (Array LevelTile)
        , triggers: List Trigger
        , startingPosition : ( Int, Int )
        , startingOrientation: Orientation
        }

type Orientation
    = North
    | East
    | South
    | West

type WorldCoordinates = WorldCoordinates

type LevelTile
    = Floor
    | Wall
    | Empty

type alias Trigger =
    { sector: (Int, Int)
    , conditions: List TriggerCondition
    , effects: List TriggerEffect
    }

type TriggerCondition
    = EnteredFrom Orientation
    | LookAngle Orientation

type TriggerEffect
    = Teleport (Int, Int)

pointOnLevel : Float -> Float -> Float -> Point3d.Point3d Length.Meters WorldCoordinates
pointOnLevel x y z =
    Point3d.meters -x y z


getTriggersAt : Level -> (Int, Int) -> List Trigger
getTriggersAt (Level levelData) sector =
    levelData.triggers
        |> List.filter (\trigger -> trigger.sector == sector)

--checkTrigger : Level -> Orientation -> Orientation -> Maybe TriggerEffect
--checkTrigger (Level levelData) goingTowards lookingAt =
--    let
--        (prevX, prevY) = Player.getSector prevPlayer
--        (newX, newY) = Player.getSector newPlayer
--        maybeOrientation = case ( sign (newX - prevX), sign (newY - prevY)) of
--            (0, -1) -> Just North
--            (1, 0) -> Just East
--            (0, 1) -> Just South
--            (-1, 0) -> Just West
--            _ -> Nothing
--    in


sign : Int -> Int
sign number =
    if number > 0 then 1
    else if number < 0 then -1
    else 0

getStartingPosition : Level -> ( Int, Int )
getStartingPosition (Level levelData) =
    levelData.startingPosition

getStartingOrientation : Level -> Orientation
getStartingOrientation (Level levelData) =
    levelData.startingOrientation

fromData : List (List LevelTile) -> List Trigger -> ( Int, Int ) -> Orientation -> Level
fromData tiles triggers startingPosition startingOrientation =
    Level
        { tiles = tiles
            |> List.map Array.fromList
            |> Array.fromList
        , triggers = triggers
        , startingPosition = startingPosition
        , startingOrientation = startingOrientation
        }

collisionOnSector : Level -> (Int, Int) -> Bool
collisionOnSector (Level levelData) (x, y) =
    levelData.tiles
        |> Array.get y
        |> Maybe.andThen (Array.get x)
        |> Maybe.map tileCollides
        |> Maybe.withDefault False

tileCollides : LevelTile -> Bool
tileCollides levelTile =
    case levelTile of
        Wall -> True
        _ -> False

view : Level -> Scene3d.Entity WorldCoordinates
view (Level levelData) =
    levelData.tiles
        |> Array.indexedMap
            (\y row ->
                Array.indexedMap
                    (\x tile ->
                        case tile of
                            Wall ->
                                Scene3d.block
                                    (Scene3d.Material.matte Color.darkRed)
                                    (Block3d.with
                                    { x1 = Length.meters (toFloat -x)
                                    , x2 = Length.meters (toFloat -(x + 1) )
                                    , y1 = Length.meters (toFloat y)
                                    , y2 = Length.meters (toFloat (y + 1))
                                    , z1 = Length.meters 0
                                    , z2 = Length.meters 1
                                    })
                            Floor ->
                                Scene3d.group
                                    [ Scene3d.block
                                           (Scene3d.Material.nonmetal { baseColor = Color.lightPurple, roughness = 0.5 })
                                           (Block3d.with
                                           { x1 = Length.meters (toFloat -x - 0.01)
                                           , x2 = Length.meters (toFloat -(x + 1) + 0.01)
                                           , y1 = Length.meters (toFloat y + 0.01)
                                           , y2 = Length.meters (toFloat (y + 1) - 0.01)
                                           , z1 = Length.meters 0
                                           , z2 = Length.meters 0.01
                                           })
                                    , Scene3d.block
                                         (Scene3d.Material.matte Color.darkBrown)
                                         (Block3d.with
                                         { x1 = Length.meters (toFloat -x)
                                         , x2 = Length.meters (toFloat -(x + 1))
                                         , y1 = Length.meters (toFloat y)
                                         , y2 = Length.meters (toFloat (y + 1))
                                         , z1 = Length.meters 0
                                         , z2 = Length.meters 0.005
                                         })
                                    ]
                            _ -> Scene3d.nothing
                    )
                    row
                    |> Array.toList
            )
        |> Array.toList
        |> List.concatMap identity
        |> Scene3d.group