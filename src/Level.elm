module Level exposing (..)

import Point3d
import Length
import Array exposing (Array)
import Scene3d
import Vector3d exposing (Vector3d)
import Point3d exposing (Point3d)
import Quantity exposing (Quantity)
import Obj.Decode exposing (ObjCoordinates)
import Orientation exposing (Orientation(..))
import SceneAssets
import Frame3d
import Axis3d
import Angle

type Level
    = Level
        { tiles : Array (Array LevelTile)
        , triggers: List Trigger
        , startingPosition : ( Int, Int )
        , startingOrientation: Orientation
        }



type alias WorldCoordinates = ObjCoordinates

type LevelTile
    = Floor
    | OpenFloor
    | Wall
    | Sign String Orientation
    | BlueWall
    | Sand
    | ToyBucket
    | Empty

type alias Trigger =
    { sector: (Int, Int)
    , conditions: List TriggerCondition
    , effects: List TriggerEffect
    }

type TriggerCondition
    = EnteredFrom Orientation
    | LookAngle Orientation
    | NegativeHeadshake
    | Nod
    | StepIn
    | CounterEquals String Int

type TriggerEffect
    = Teleport (Int, Int)
    | NextLevel
    | ChangeTile (Int, Int) LevelTile
    | CreateTrigger Trigger
    | RemoveAllTriggersInSector (Int, Int)
    | IncrementCounter String
    | DecrementCounter String
    | PlaySound String

pointOnLevel : Float -> Float -> Float -> Point3d.Point3d Length.Meters WorldCoordinates
pointOnLevel x y z =
    Point3d.meters -x y z

--tileToRequiredTextures : LevelTile -> List TextureToLoad
--tileToRequiredTextures tile =
--    case tile of
--        Floor ->
--            [ Textures.TextureColor "CheckerFloor.jpg"
--            , Textures.TextureColor "OfficeCeiling005_4K_Color.jpg"
--            ]
--        Wall ->
--            [ Textures.TextureColor "Bricks021_1K-JPG_Color.jpg"
--            , Textures.TextureFloat "Bricks021_1K-JPG_Roughness.jpg"
--            ]
--        Sign fileName text _ ->
--            [ Textures.GenerateSign fileName text ]
--        BlueWall ->
--            [ Textures.TextureColor "CorrugatedSteel007B_1K-JPG_Color.jpg"
--            , Textures.TextureFloat "CorrugatedSteel007B_1K-JPG_Metalness.jpg"
--            , Textures.TextureFloat "CorrugatedSteel007B_1K-JPG_Roughness.jpg"
--            ]
--        Empty -> []


getTriggersAt : Level -> (Int, Int) -> List Trigger
getTriggersAt (Level levelData) sector =
    levelData.triggers
        |> List.filter (\trigger -> trigger.sector == sector)

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

worldCoordinateToSector : Point3d Length.Meters WorldCoordinates -> (Int, Int)
worldCoordinateToSector point =
    let
        p = Point3d.toMeters point
    in
        (floor -p.x, floor p.y) -- ceiling for negativeX ?

cylinderCollisionSector : Level -> Length.Length -> Point3d Length.Meters WorldCoordinates -> Maybe (Int, Int)
cylinderCollisionSector level radius center =
    cylinderCollisionSectors level (Length.inMeters radius) center
        |> List.head

cylinderCollisionSectors : Level -> Float -> Point3d Length.Meters WorldCoordinates -> List (Int, Int)
cylinderCollisionSectors (Level levelData) radius center =
    let
        (centerSectorX, centerSectorY) = worldCoordinateToSector center
        centerInMeters = Point3d.toMeters center

        neighborSectors =
            [ (centerSectorX, centerSectorY - 1)
            , (centerSectorX - 1, centerSectorY)
            , (centerSectorX + 1, centerSectorY)
            , (centerSectorX, centerSectorY + 1)
            , (centerSectorX - 1, centerSectorY - 1)
            , (centerSectorX + 1, centerSectorY - 1)
            , (centerSectorX - 1, centerSectorY + 1)
            , (centerSectorX + 1, centerSectorY + 1)
            ]
    in
        neighborSectors
            |> List.filter (collisionOnSector (Level levelData))
            |> List.filter
                (\(x, y) ->
                    let
                        minX = maxX - 1
                        maxX = -(toFloat x)
                        minY = toFloat y
                        maxY = minY + 1

                        closestCollisionX = clamp minX maxX centerInMeters.x
                        closestCollisionY = clamp minY maxY centerInMeters.y

                        realDistance = sqrt ((closestCollisionX - centerInMeters.x) ^ 2 + (closestCollisionY - centerInMeters.y) ^ 2)
                    in
                        realDistance <= radius
                )



sectorsDistance : (Int, Int) -> (Int, Int) -> Float
sectorsDistance (x1, y1) (x2, y2) =
    sqrt (toFloat ((x2 - x1) ^ 2 + (y2 - y1) ^ 2))

translatePointByVector : Point3d Length.Meters WorldCoordinates -> Vector3d Length.Meters WorldCoordinates -> Point3d Length.Meters WorldCoordinates
translatePointByVector p v =
    p
        |> Point3d.toMeters
        |> Vector3d.fromMeters
        |> Vector3d.plus v
        |> Vector3d.toMeters
        |> Point3d.fromMeters

sqrt2 = sqrt 2

applyCollision : Length.Length -> Level -> Point3d Length.Meters WorldCoordinates -> Vector3d Length.Meters WorldCoordinates -> Vector3d Length.Meters WorldCoordinates
applyCollision bodyRadius (Level levelData) position v =
    let
        (cx, cy) = worldCoordinateToSector position

        desiredPosition = translatePointByVector position v
        sectorCollisionsAfterMove = cylinderCollisionSectors (Level levelData) (Length.inMeters bodyRadius) desiredPosition
        nearestCollidingSector =
            sectorCollisionsAfterMove
                |> List.map (\(x, y) -> (sectorsDistance (x, y) (cx, cy), (x, y)))
                |> List.sortBy Tuple.first
                |> List.head
                |> Maybe.map Tuple.second

    in
    case nearestCollidingSector of
        Nothing -> v
        Just (sx, sy) ->
            -- Slide
            let
                dx = sx - cx
                dy = sy - cy

                collisionNormal =
                    Vector3d.fromMeters
                        ( if dx < 0 && dy < 0 then -- TL
                            { x = -sqrt2, y = sqrt2, z = 0 }
                        else if dx == 0 && dy < 0 then -- T
                            { x = 0, y = 1, z = 0 }
                        else if (dx > 0 && dy < 0) then -- TR
                            { x = sqrt2, y = sqrt2, z = 0 }
                        else if (dx < 0 && dy == 0) then -- L
                            { x = -1, y = 0, z = 0 }
                        else if (dx > 0 && dy == 0) then -- R
                            { x = 1, y = 0, z = 0 }
                        else if (dx < 0 && dy > 0) then -- BL
                            { x = -sqrt2, y = -sqrt2, z = 0 }
                        else if (dx == 0 && dy > 0) then -- B
                            { x = 0, y = -1, z = 0 }
                        else -- BR
                            { x = sqrt2, y = -sqrt2, z = 0 }
                        )

                velocityProjection = Vector3d.dot v collisionNormal
                    |> Quantity.unwrap

            in
                Vector3d.minus (Vector3d.scaleBy velocityProjection collisionNormal) v

tileCollides : LevelTile -> Bool
tileCollides levelTile =
    case levelTile of
        Wall -> True
        Sign _ _ -> True
        BlueWall -> True
        _ -> False

updateTile : (Int, Int) -> LevelTile -> Level -> Level
updateTile (x, y) newTile (Level levelData) =
    Level { levelData | tiles =
        Array.indexedMap
            (\rowIndex row ->
                if rowIndex == y then
                    Array.indexedMap (\tileIndex tile -> if tileIndex == x then newTile else tile) row
                else
                    row
            ) levelData.tiles
    }


addTrigger : Trigger -> Level -> Level
addTrigger trigger (Level levelData) =
    Level { levelData | triggers = trigger :: levelData.triggers }

removeAllTriggersAtSector : (Int, Int) -> Level -> Level
removeAllTriggersAtSector sector (Level levelData) =
    Level { levelData | triggers = List.filter (\trigger -> trigger.sector /= sector) levelData.triggers }

view : SceneAssets.Model -> Level -> Scene3d.Entity WorldCoordinates
view sceneAssets (Level levelData) =
    levelData.tiles
        |> Array.indexedMap
            (\y row ->
                Array.indexedMap
                    (\x tile ->
                        let
                            worldX = -(toFloat x) - 0.5
                            worldY = (toFloat y) + 0.5
                            tileCenter = (Frame3d.atPoint (Point3d.meters worldX worldY 0))
                        in
                        case tile of
                            Wall ->
                                SceneAssets.wallBlock sceneAssets
                                    |> Scene3d.placeIn tileCenter

                            BlueWall ->
                                SceneAssets.blueWallBlock sceneAssets
                                    |> Scene3d.placeIn tileCenter

                            Floor ->
                                Scene3d.group
                                    [ SceneAssets.floorTile sceneAssets
                                    , SceneAssets.ceilingTile sceneAssets
                                    ]
                                    |> Scene3d.placeIn tileCenter

                            Sand ->
                                 SceneAssets.sandTile sceneAssets
                                    |> Scene3d.placeIn tileCenter

                            Sign signName orientation ->
                                let
                                    rotationAngle =
                                        case orientation of
                                            South -> 0
                                            West -> 90
                                            North -> 180
                                            East -> -90

                                in
                                Scene3d.group
                                    [ SceneAssets.sign sceneAssets signName
                                        |> Scene3d.translateBy (Vector3d.fromMeters { x = 0, y = 0.51, z = 0 })
                                        |> Scene3d.rotateAround Axis3d.z (Angle.degrees rotationAngle)
                                    , SceneAssets.wallBlock sceneAssets
                                    ]
                                    |> Scene3d.placeIn tileCenter

                            ToyBucket ->
                                SceneAssets.toyBucket sceneAssets
                                    |> Scene3d.placeIn tileCenter

                            _ -> Scene3d.nothing
                    )
                    row
                    |> Array.toList
            )
        |> Array.toList
        |> List.concatMap identity
        |> Scene3d.group
