module Level exposing (..)

import Angle
import Array exposing (Array)
import Axis3d
import Block3d
import BreakableWall
import Castle
import Color exposing (Color)
import Dict exposing (Dict)
import Frame3d
import Length exposing (Length)
import Obj.Decode exposing (ObjCoordinates)
import Orientation exposing (Orientation(..))
import Point2d
import Point3d exposing (Point3d)
import Quantity exposing (Quantity)
import Scene3d
import Scene3d.Material
import SceneAssets
import Sound
import Terms
import Vector3d exposing (Vector3d)


type Level
    = Level
        { partitions : Array Partition
        , groundSounds : Dict ( Int, Int ) GroundSound
        , collisions : Dict ( Int, Int ) Bool
        , dynamicCollisions : List ( Int, Int )
        , triggers : List Trigger
        , sectorYToPartitionIndex : Dict Int Int
        , startingPosition : ( Int, Int )
        , startingOrientation : Orientation
        }


type alias Partition =
    { staticEntities : List StaticEntity
    , dynamicEntities : List DynamicEntity
    }


type alias StaticEntity =
    { sector : ( Int, Int )
    , tile : LevelTile
    }


type DynamicEntity
    = TermsEntity ( Int, Int ) Terms.Model
    | BreakableWallEntity ( Int, Int ) BreakableWall.Model


type alias WorldCoordinates =
    ObjCoordinates


type LevelInteractionResult
    = NoInteraction
    | LevelCollision (Vector3d Length.Meters WorldCoordinates)
    | LevelUpdated Level


type LevelTile
    = Floor
    | OpenFloor
    | Wall
    | Sign String Orientation
    | BlueWall
    | Sand
    | ToyBucket
    | InvisibleWall LevelTile
    | BigCastle
    | Chair
    | Sandbox Bool
    | Terms
    | BreakableWall
    | BlackWall
    | BlackFloor
    | Empty


type GroundSound
    = SolidFloor
    | SandGround
    | VoidGround


type alias Trigger =
    { sector : ( Int, Int )
    , conditions : List TriggerCondition
    , effects : List TriggerEffect
    }


type TriggerCondition
    = EnteredFrom Orientation
    | LookAngle Orientation
    | LookingAtGround
    | NegativeHeadshake
    | Nod
    | StepIn
    | CounterEquals String Int


type TriggerEffect
    = Teleport ( Int, Int )
    | NextLevel
    | ChangeTile ( Int, Int ) LevelTile
    | CreateTrigger Trigger
    | RemoveAllTriggersInSector ( Int, Int )
    | RemoveAllTriggersInSectors (List ( Int, Int ))
    | IncrementCounter String
    | DecrementCounter String
    | PlaySound String
    | InitFog Color Length
    | SitDown
    | OpenTerms ( Int, Int )
    | PlayMusic String
    | StartNarration Int
    | ShowGameEndingScreen


getGroundSound : Level -> ( Int, Int ) -> GroundSound
getGroundSound (Level levelData) sector =
    Dict.get sector levelData.groundSounds
        |> Maybe.withDefault VoidGround


pointOnLevel : Float -> Float -> Float -> Point3d.Point3d Length.Meters WorldCoordinates
pointOnLevel x y z =
    Point3d.meters -x y z


getTriggersAt : Level -> ( Int, Int ) -> List Trigger
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
    let
        partitioningData =
            tiles
                |> List.indexedMap Tuple.pair
                |> List.foldl
                    (\( y, row ) { tilePartitions, currentPartition, yDict, partitionYOffset, partitionIndex } ->
                        if List.all ((==) Empty) row then
                            { tilePartitions = tilePartitions ++ [ currentPartition ]
                            , currentPartition = []
                            , yDict = yDict
                            , partitionYOffset = y + 1
                            , partitionIndex = partitionIndex + 1
                            }

                        else
                            { tilePartitions = tilePartitions
                            , currentPartition = currentPartition ++ List.indexedMap (\x tile -> ( ( x, y ), tile )) row
                            , yDict = Dict.insert y partitionIndex yDict
                            , partitionYOffset = partitionYOffset
                            , partitionIndex = partitionIndex
                            }
                    )
                    { tilePartitions = [], currentPartition = [], yDict = Dict.empty, partitionYOffset = 0, partitionIndex = 0 }

        groundSounds =
            partitioningData.tilePartitions
                |> List.concatMap identity
                |> List.map
                    (\( sector, tile ) ->
                        ( sector
                        , case tile of
                            OpenFloor ->
                                SolidFloor

                            Floor ->
                                SolidFloor

                            BlackFloor ->
                                SolidFloor

                            Terms ->
                                SolidFloor

                            Sand ->
                                SandGround

                            ToyBucket ->
                                SandGround

                            _ ->
                                VoidGround
                        )
                    )
                |> Dict.fromList

        partitions =
            partitioningData.tilePartitions
                |> List.map
                    (List.foldl
                        (\( sector, tile ) { staticEntities, dynamicEntities } ->
                            case tile of
                                Terms ->
                                    { staticEntities = staticEntities, dynamicEntities = TermsEntity sector Terms.init :: dynamicEntities }

                                BreakableWall ->
                                    { staticEntities = staticEntities, dynamicEntities = BreakableWallEntity sector BreakableWall.init :: dynamicEntities }

                                Empty ->
                                    { staticEntities = staticEntities, dynamicEntities = dynamicEntities }

                                _ ->
                                    { staticEntities = { sector = sector, tile = tile } :: staticEntities, dynamicEntities = dynamicEntities }
                        )
                        { staticEntities = [], dynamicEntities = [] }
                    )
    in
    Level
        { partitions = Array.fromList partitions
        , sectorYToPartitionIndex = partitioningData.yDict
        , dynamicCollisions =
            partitions
                |> List.concatMap .dynamicEntities
                |> List.map
                    (\dynamicEntity ->
                        case dynamicEntity of
                            TermsEntity sector _ ->
                                sector

                            BreakableWallEntity sector _ ->
                                sector
                    )
        , collisions =
            partitions
                |> List.concatMap .staticEntities
                |> List.map (\tileEntity -> ( tileEntity.sector, tileCollides tileEntity.tile ))
                |> Dict.fromList
        , triggers = triggers
        , groundSounds = groundSounds
        , startingPosition = startingPosition
        , startingOrientation = startingOrientation
        }


partitionForSector ( x, y ) { partitions, sectorYToPartitionIndex } =
    Dict.get y sectorYToPartitionIndex
        |> Maybe.andThen (\partitionIndex -> Array.get partitionIndex partitions)
        |> Maybe.withDefault { staticEntities = [], dynamicEntities = [] }


collisionOnSector : Level -> ( Int, Int ) -> Bool
collisionOnSector (Level levelData) sector =
    if List.any ((==) sector) levelData.dynamicCollisions then
        levelData
            |> partitionForSector sector
            |> .dynamicEntities
            |> List.any
                (\dynamicEntity ->
                    case dynamicEntity of
                        TermsEntity entitySector terms ->
                            entitySector == sector && Terms.doesCollide terms

                        BreakableWallEntity entitySector breakableWall ->
                            entitySector == sector && not (BreakableWall.isBroken breakableWall)
                )

    else
        levelData.collisions
            |> Dict.get sector
            |> Maybe.withDefault False


worldCoordinateToSector : Point3d Length.Meters WorldCoordinates -> ( Int, Int )
worldCoordinateToSector point =
    let
        p =
            Point3d.toMeters point
    in
    ( floor -p.x, floor p.y )



-- ceiling for negativeX ?


interactAsCylinder : Length.Length -> Point3d Length.Meters WorldCoordinates -> Vector3d.Vector3d Length.Meters WorldCoordinates -> Level -> ( LevelInteractionResult, Cmd msg )
interactAsCylinder radius center v level =
    let
        adjustedVector =
            applyCollision radius level center v

        targetSectorCollision =
            center
                |> Point3d.translateBy v
                |> cylinderCollisionSector level radius

        adjustedSectorCollision =
            center
                |> Point3d.translateBy adjustedVector
                |> cylinderCollisionSector level radius
    in
    case targetSectorCollision of
        Just sector ->
            case ( dynamicEntityAtSector level sector, adjustedSectorCollision ) of
                ( Just (BreakableWallEntity _ _), _ ) ->
                    ( LevelUpdated (breakWall level sector center v), Sound.playSound "glass-break.mp3" )

                ( _, Just _ ) ->
                    ( LevelCollision Vector3d.zero, Cmd.none )

                _ ->
                    ( LevelCollision adjustedVector, Cmd.none )

        Nothing ->
            ( NoInteraction, Cmd.none )


mapDynamicEntity : (DynamicEntity -> DynamicEntity) -> ( Int, Int ) -> Level -> Level
mapDynamicEntity f sector level =
    let
        (Level { sectorYToPartitionIndex }) =
            level
    in
    case Dict.get (Tuple.second sector) sectorYToPartitionIndex of
        Just partitionIndex ->
            mapPartition
                (\partition ->
                    { partition
                        | dynamicEntities =
                            List.map
                                (\entity ->
                                    if getDynamicEntitySector entity == sector then
                                        f entity

                                    else
                                        entity
                                )
                                partition.dynamicEntities
                    }
                )
                partitionIndex
                level

        Nothing ->
            level


mapStaticEntity : (StaticEntity -> StaticEntity) -> ( Int, Int ) -> Level -> Level
mapStaticEntity f sector level =
    let
        (Level { sectorYToPartitionIndex }) =
            level
    in
    case Dict.get (Tuple.second sector) sectorYToPartitionIndex of
        Just partitionIndex ->
            mapPartition
                (\partition ->
                    { partition
                        | staticEntities =
                            List.map
                                (\entity ->
                                    if entity.sector == sector then
                                        f entity

                                    else
                                        entity
                                )
                                partition.staticEntities
                    }
                )
                partitionIndex
                level

        Nothing ->
            level


getDynamicEntitySector : DynamicEntity -> ( Int, Int )
getDynamicEntitySector dynamicEntity =
    case dynamicEntity of
        BreakableWallEntity sector _ ->
            sector

        TermsEntity sector _ ->
            sector


mapPartition : (Partition -> Partition) -> Int -> Level -> Level
mapPartition f index (Level levelData) =
    case Array.get index levelData.partitions of
        Just partition ->
            Level { levelData | partitions = Array.set index (f partition) levelData.partitions }

        Nothing ->
            Level levelData


breakWall : Level -> ( Int, Int ) -> Point3d.Point3d Length.Meters WorldCoordinates -> Vector3d.Vector3d Length.Meters WorldCoordinates -> Level
breakWall level sector collisionPoint speedVector =
    mapDynamicEntity
        (\dynamicEntity ->
            case dynamicEntity of
                BreakableWallEntity entitySector breakableWall ->
                    let
                        { x, y, z } =
                            Point3d.toMeters collisionPoint

                        offsetX =
                            -x - toFloat (floor -x) - 0.5
                    in
                    BreakableWallEntity entitySector (BreakableWall.break (Point2d.fromMeters { x = offsetX, y = 1 - z }) speedVector breakableWall)

                _ ->
                    dynamicEntity
        )
        sector
        level


dynamicEntityAtSector : Level -> ( Int, Int ) -> Maybe DynamicEntity
dynamicEntityAtSector (Level level) sector =
    partitionForSector sector level
        |> .dynamicEntities
        |> List.filter
            (\dynamicEntity ->
                case dynamicEntity of
                    TermsEntity entitySector _ ->
                        sector == entitySector

                    BreakableWallEntity entitySector _ ->
                        sector == entitySector
            )
        |> List.head


cylinderCollisionSector : Level -> Length.Length -> Point3d Length.Meters WorldCoordinates -> Maybe ( Int, Int )
cylinderCollisionSector level radius center =
    cylinderCollisionSectors level (Length.inMeters radius) center
        |> List.head


cylinderCollisionSectors : Level -> Float -> Point3d Length.Meters WorldCoordinates -> List ( Int, Int )
cylinderCollisionSectors (Level levelData) radius center =
    let
        ( centerSectorX, centerSectorY ) =
            worldCoordinateToSector center

        centerInMeters =
            Point3d.toMeters center

        neighborSectors =
            [ ( centerSectorX, centerSectorY - 1 )
            , ( centerSectorX - 1, centerSectorY )
            , ( centerSectorX + 1, centerSectorY )
            , ( centerSectorX, centerSectorY + 1 )
            , ( centerSectorX - 1, centerSectorY - 1 )
            , ( centerSectorX + 1, centerSectorY - 1 )
            , ( centerSectorX - 1, centerSectorY + 1 )
            , ( centerSectorX + 1, centerSectorY + 1 )
            ]
    in
    neighborSectors
        |> List.filter (collisionOnSector (Level levelData))
        |> List.filter
            (\( x, y ) ->
                let
                    minX =
                        maxX - 1

                    maxX =
                        -(toFloat x)

                    minY =
                        toFloat y

                    maxY =
                        minY + 1

                    closestCollisionX =
                        clamp minX maxX centerInMeters.x

                    closestCollisionY =
                        clamp minY maxY centerInMeters.y

                    realDistance =
                        sqrt ((closestCollisionX - centerInMeters.x) ^ 2 + (closestCollisionY - centerInMeters.y) ^ 2)
                in
                realDistance <= radius
            )


sectorsDistance : ( Int, Int ) -> ( Int, Int ) -> Float
sectorsDistance ( x1, y1 ) ( x2, y2 ) =
    sqrt (toFloat ((x2 - x1) ^ 2 + (y2 - y1) ^ 2))


translatePointByVector : Point3d Length.Meters WorldCoordinates -> Vector3d Length.Meters WorldCoordinates -> Point3d Length.Meters WorldCoordinates
translatePointByVector p v =
    p
        |> Point3d.toMeters
        |> Vector3d.fromMeters
        |> Vector3d.plus v
        |> Vector3d.toMeters
        |> Point3d.fromMeters


sqrt2 =
    sqrt 2


applyCollision : Length.Length -> Level -> Point3d Length.Meters WorldCoordinates -> Vector3d Length.Meters WorldCoordinates -> Vector3d Length.Meters WorldCoordinates
applyCollision bodyRadius (Level levelData) position v =
    let
        ( cx, cy ) =
            worldCoordinateToSector position

        desiredPosition =
            translatePointByVector position v

        sectorCollisionsAfterMove =
            cylinderCollisionSectors (Level levelData) (Length.inMeters bodyRadius) desiredPosition

        nearestCollidingSector =
            sectorCollisionsAfterMove
                |> List.map (\( x, y ) -> ( sectorsDistance ( x, y ) ( cx, cy ), ( x, y ) ))
                |> List.sortBy Tuple.first
                |> List.head
                |> Maybe.map Tuple.second
    in
    case nearestCollidingSector of
        Nothing ->
            v

        Just ( sx, sy ) ->
            -- Slide
            let
                dx =
                    sx - cx

                dy =
                    sy - cy

                collisionNormal =
                    Vector3d.fromMeters
                        (if dx < 0 && dy < 0 then
                            -- TL
                            { x = -sqrt2, y = sqrt2, z = 0 }

                         else if dx == 0 && dy < 0 then
                            -- T
                            { x = 0, y = 1, z = 0 }

                         else if dx > 0 && dy < 0 then
                            -- TR
                            { x = sqrt2, y = sqrt2, z = 0 }

                         else if dx < 0 && dy == 0 then
                            -- L
                            { x = -1, y = 0, z = 0 }

                         else if dx > 0 && dy == 0 then
                            -- R
                            { x = 1, y = 0, z = 0 }

                         else if dx < 0 && dy > 0 then
                            -- BL
                            { x = -sqrt2, y = -sqrt2, z = 0 }

                         else if dx == 0 && dy > 0 then
                            -- B
                            { x = 0, y = -1, z = 0 }

                         else
                            -- BR
                            { x = sqrt2, y = -sqrt2, z = 0 }
                        )

                velocityProjection =
                    Vector3d.dot v collisionNormal
                        |> Quantity.unwrap
            in
            Vector3d.minus (Vector3d.scaleBy velocityProjection collisionNormal) v


tileCollides : LevelTile -> Bool
tileCollides levelTile =
    case levelTile of
        Wall ->
            True

        Sign _ _ ->
            True

        BlueWall ->
            True

        BlackWall ->
            True

        InvisibleWall _ ->
            True

        Sandbox _ ->
            True

        _ ->
            False


updateTile : ( Int, Int ) -> LevelTile -> Level -> Level
updateTile sector newTile level =
    let
        (Level levelData) =
            mapStaticEntity (always { sector = sector, tile = newTile }) sector level
    in
    Level { levelData | collisions = Dict.insert sector (tileCollides newTile) levelData.collisions }


addTrigger : Trigger -> Level -> Level
addTrigger trigger (Level levelData) =
    Level { levelData | triggers = trigger :: levelData.triggers }


removeAllTriggersAtSector : ( Int, Int ) -> Level -> Level
removeAllTriggersAtSector sector (Level levelData) =
    Level { levelData | triggers = List.filter (\trigger -> trigger.sector /= sector) levelData.triggers }


removeAllTriggersAtSectors : List ( Int, Int ) -> Level -> Level
removeAllTriggersAtSectors sectors (Level levelData) =
    Level { levelData | triggers = List.filter (\trigger -> List.all ((/=) trigger.sector) sectors) levelData.triggers }



--sectorCenter : (Int, Int) -> Point3d Length.Meters WorldCoordinates


sectorCenter ( x, y ) =
    let
        worldX =
            -(toFloat x) - 0.5

        worldY =
            toFloat y + 0.5
    in
    Frame3d.atPoint (Point3d.meters worldX worldY 0)


viewTile : SceneAssets.Model -> ( Int, Int ) -> LevelTile -> Scene3d.Entity WorldCoordinates
viewTile sceneAssets ( x, y ) tile =
    let
        worldX =
            -(toFloat x) - 0.5

        worldY =
            toFloat y + 0.5

        tileCenter =
            Frame3d.atPoint (Point3d.meters worldX worldY 0)
    in
    case tile of
        Wall ->
            SceneAssets.wallBlock sceneAssets
                |> Scene3d.placeIn tileCenter

        BlueWall ->
            SceneAssets.blueWallBlock sceneAssets
                |> Scene3d.placeIn tileCenter

        BlackWall ->
            Scene3d.block (Scene3d.Material.color Color.black)
                (Block3d.with
                    { x1 = Length.meters -0.5
                    , x2 = Length.meters 0.5
                    , y1 = Length.meters -0.5
                    , y2 = Length.meters 0.5
                    , z1 = Length.meters 0
                    , z2 = Length.meters 1
                    }
                )
                |> Scene3d.placeIn tileCenter

        BlackFloor ->
            Scene3d.group
                [ Scene3d.quad (Scene3d.Material.color Color.black)
                    (Point3d.unsafe { x = -0.5, y = -0.5, z = 0 })
                    (Point3d.unsafe { x = 0.5, y = -0.5, z = 0 })
                    (Point3d.unsafe { x = 0.5, y = 0.5, z = 0 })
                    (Point3d.unsafe { x = -0.5, y = 0.5, z = 0 })
                , Scene3d.quad (Scene3d.Material.color Color.black)
                    (Point3d.unsafe { x = -0.5, y = -0.5, z = 1 })
                    (Point3d.unsafe { x = 0.5, y = -0.5, z = 1 })
                    (Point3d.unsafe { x = 0.5, y = 0.5, z = 1 })
                    (Point3d.unsafe { x = -0.5, y = 0.5, z = 1 })
                ]
                |> Scene3d.placeIn tileCenter

        Floor ->
            Scene3d.group
                [ SceneAssets.floorTile sceneAssets
                , SceneAssets.ceilingTile sceneAssets
                ]
                |> Scene3d.placeIn tileCenter

        OpenFloor ->
            SceneAssets.floorTile sceneAssets
                |> Scene3d.placeIn tileCenter

        Sand ->
            SceneAssets.sandTile sceneAssets
                |> Scene3d.placeIn tileCenter

        Sign signName orientation ->
            let
                rotationAngle =
                    case orientation of
                        South ->
                            0

                        West ->
                            -90

                        North ->
                            180

                        East ->
                            90
            in
            Scene3d.group
                [ SceneAssets.sign sceneAssets signName
                    |> Scene3d.translateBy (Vector3d.fromMeters { x = 0, y = 0.51, z = 0 })
                    |> Scene3d.rotateAround Axis3d.z (Angle.degrees rotationAngle)
                , SceneAssets.wallBlock sceneAssets
                ]
                |> Scene3d.placeIn tileCenter

        ToyBucket ->
            Scene3d.group
                [ SceneAssets.toyBucket sceneAssets
                , SceneAssets.sandTile sceneAssets
                ]
                |> Scene3d.placeIn tileCenter

        InvisibleWall innerTile ->
            viewTile sceneAssets ( x, y ) innerTile

        BigCastle ->
            Scene3d.group
                [ Castle.view sceneAssets True
                    |> Scene3d.rotateAround Axis3d.z (Angle.degrees -90)
                    |> Scene3d.scaleAbout Point3d.origin 3
                    |> Scene3d.placeIn tileCenter
                , viewTile sceneAssets ( x, y ) Sand
                ]

        Sandbox withCastle ->
            Scene3d.group
                ([ SceneAssets.sandbox sceneAssets
                    |> Scene3d.placeIn tileCenter
                 , SceneAssets.ceilingTile sceneAssets
                    |> Scene3d.placeIn tileCenter
                 ]
                    ++ (if withCastle then
                            [ Castle.view sceneAssets False
                                |> Scene3d.rotateAround Axis3d.z (Angle.degrees 90)
                                |> Scene3d.scaleAbout Point3d.origin 0.05
                                |> Scene3d.placeIn (Frame3d.atPoint (Point3d.meters worldX worldY 0.08))
                            ]

                        else
                            []
                       )
                )

        Chair ->
            SceneAssets.chair sceneAssets
                |> Scene3d.placeIn tileCenter

        _ ->
            Scene3d.nothing


view : SceneAssets.Model -> ( Int, Int ) -> Level -> Scene3d.Entity WorldCoordinates
view sceneAssets sector (Level levelData) =
    let
        { staticEntities, dynamicEntities } =
            partitionForSector sector levelData
    in
    [ staticEntities
        |> List.map (\tileEntity -> viewTile sceneAssets tileEntity.sector tileEntity.tile)
    , dynamicEntities
        |> List.map
            (\dynamicEntity ->
                case dynamicEntity of
                    TermsEntity ( x, y ) terms ->
                        Scene3d.group
                            [ Terms.view sceneAssets terms
                                |> Scene3d.placeIn (Frame3d.atPoint (Point3d.meters (-(toFloat x) - 0.5) (toFloat y + 1) 0))
                            , viewTile sceneAssets ( x, y ) BlackFloor
                            ]

                    BreakableWallEntity ( x, y ) breakableWall ->
                        Scene3d.group
                            [ BreakableWall.view sceneAssets breakableWall
                                |> Scene3d.placeIn (Frame3d.atPoint (Point3d.meters -(toFloat x) (toFloat y + 1) 0))
                            , viewTile sceneAssets ( x, y ) Floor
                            ]
            )
    ]
        |> List.concat
        |> Scene3d.group


update : Float -> Level -> Level
update delta (Level levelData) =
    Level
        { levelData
            | partitions =
                levelData.partitions
                    |> Array.map
                        (\partition ->
                            { partition
                                | dynamicEntities =
                                    List.map
                                        (\dynamicEntity ->
                                            case dynamicEntity of
                                                TermsEntity sector termsState ->
                                                    TermsEntity sector (Terms.update delta termsState)

                                                BreakableWallEntity sector breakableWall ->
                                                    BreakableWallEntity sector (BreakableWall.update delta breakableWall)
                                        )
                                        partition.dynamicEntities
                            }
                        )
        }


openTerms : Level -> ( Int, Int ) -> Level
openTerms level sector =
    mapDynamicEntity
        (\dynamicEntity ->
            case dynamicEntity of
                TermsEntity entitySector termsState ->
                    TermsEntity entitySector (Terms.open termsState)

                _ ->
                    dynamicEntity
        )
        sector
        level
