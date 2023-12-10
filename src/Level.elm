module Level exposing
    ( LevelInteractionResult(..)
    , Model
    , activateTile
    , addTrigger
    , fromData
    , getGroundSound
    , getSignTextAt
    , getTileAt
    , getTriggersAt
    , initPlayer
    , interact
    , mapTile
    , removeAllGlobalTriggers
    , removeAllTriggersAtSector
    , removeAllTriggersAtSectors
    , update
    , updateTile
    , view
    )

import Array exposing (Array)
import Assets exposing (Dependency)
import Coordinates exposing (SectorCoordinates, WorldCoordinates)
import Dict exposing (Dict)
import Frame3d
import Length
import LevelTile
import List.Unique
import Orientation exposing (Orientation)
import Player exposing (Player)
import Point3d exposing (Point3d)
import Quantity
import Scene3d
import Trigger exposing (Trigger)
import Vector3d exposing (Vector3d)


type Model
    = Level
        { partitions : Array Partition
        , sectorYToPartitionIndex : Dict Int Int
        , triggersForSector : Dict SectorCoordinates (List Trigger)
        , globalTriggers : List Trigger
        , playerStartPosition : SectorCoordinates
        , playerStartingOrientation : Orientation
        }


type alias Partition =
    Dict SectorCoordinates LevelTile.Model


type alias LevelInputData =
    { tiles : List (List LevelTile.Model)
    , triggers : List Trigger
    , playerStartPosition : SectorCoordinates
    , playerStartingOrientation : Orientation
    }


emptyPartition : Partition
emptyPartition =
    Dict.empty


view : Assets.Model -> Model -> SectorCoordinates -> Scene3d.Entity WorldCoordinates
view assets (Level { partitions, sectorYToPartitionIndex }) viewSector =
    let
        currentPartition =
            viewSector
                |> Tuple.second
                |> (\y ->
                        Dict.get y sectorYToPartitionIndex
                            |> Maybe.withDefault 0
                            |> (\partitionIndex ->
                                    Array.get partitionIndex partitions
                                        |> Maybe.withDefault emptyPartition
                               )
                   )
    in
    currentPartition
        |> Dict.toList
        |> List.map
            (\( sector, tile ) ->
                tile
                    |> LevelTile.view assets
                    |> Scene3d.placeIn (Frame3d.atPoint (Coordinates.sectorToWorldPosition sector))
            )
        |> Scene3d.group


fromData : LevelInputData -> ( Model, List Dependency )
fromData { tiles, triggers, playerStartPosition, playerStartingOrientation } =
    let
        uniqueTilesUsed =
            List.concatMap identity tiles
                ++ List.concatMap Trigger.tilesUsed triggers
                |> List.Unique.filterDuplicates

        tileDependencies =
            List.concatMap LevelTile.dependencies uniqueTilesUsed

        triggerDependencies =
            List.concatMap Trigger.dependencies triggers

        dependencies =
            tileDependencies ++ triggerDependencies

        partitioningData =
            tiles
                |> List.indexedMap Tuple.pair
                |> List.foldl
                    (\( y, row ) { tilePartitions, currentPartition, yDict, partitionYOffset, partitionIndex } ->
                        if List.all ((==) LevelTile.empty) row then
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

        triggersForSector =
            triggers
                |> List.foldl
                    (\trigger sectorTriggersDict ->
                        trigger
                            |> Trigger.narrowedToSector
                            |> Maybe.map
                                (\sector ->
                                    Dict.update sector
                                        (\maybeTriggersAtSector ->
                                            case maybeTriggersAtSector of
                                                Just existingTriggers ->
                                                    Just (trigger :: existingTriggers)

                                                Nothing ->
                                                    Just [ trigger ]
                                        )
                                        sectorTriggersDict
                                )
                            |> Maybe.withDefault sectorTriggersDict
                    )
                    Dict.empty

        globalTriggers =
            List.filter Trigger.isGlobalTrigger triggers
    in
    ( Level
        { playerStartPosition = playerStartPosition
        , playerStartingOrientation = playerStartingOrientation
        , triggersForSector = triggersForSector
        , globalTriggers = globalTriggers
        , sectorYToPartitionIndex = partitioningData.yDict
        , partitions =
            partitioningData.tilePartitions
                |> List.map Dict.fromList
                |> Array.fromList
        }
    , dependencies
    )


initPlayer : Float -> Model -> Player
initPlayer mouseSensitivity (Level { playerStartPosition, playerStartingOrientation }) =
    Player.init mouseSensitivity playerStartPosition playerStartingOrientation


getTriggersAt : Model -> SectorCoordinates -> List Trigger
getTriggersAt (Level { triggersForSector, globalTriggers }) sector =
    Dict.get sector triggersForSector
        |> Maybe.withDefault []
        |> (++) globalTriggers


getGroundSound : Model -> SectorCoordinates -> LevelTile.GroundSound
getGroundSound level sector =
    getTileAt level sector
        |> LevelTile.groundSound


getTileAt : Model -> SectorCoordinates -> LevelTile.Model
getTileAt (Level { partitions, sectorYToPartitionIndex }) ( x, y ) =
    Dict.get y sectorYToPartitionIndex
        |> Maybe.andThen (\partitionIndex -> Array.get partitionIndex partitions)
        |> Maybe.withDefault emptyPartition
        |> Dict.get ( x, y )
        |> Maybe.withDefault LevelTile.empty


update : Float -> Model -> Model
update delta (Level levelData) =
    Level
        { levelData
            | partitions = Array.map (\partition -> Dict.map (\sector tile -> LevelTile.update delta tile) partition) levelData.partitions
        }


type LevelInteractionResult
    = NoInteraction
    | LevelCollision (Vector3d Length.Meters WorldCoordinates)
    | LevelUpdated Model


collisionOnSector : Model -> Bool -> SectorCoordinates -> Bool
collisionOnSector model upsideDown sector =
    sector
        |> getTileAt model
        |> LevelTile.collision
        |> (\collisionType ->
                case collisionType of
                    LevelTile.NoCollision ->
                        False

                    LevelTile.FloorCollision ->
                        not upsideDown

                    LevelTile.Collision ->
                        True
           )


interact : Model -> Player -> Vector3d Length.Meters WorldCoordinates -> ( LevelInteractionResult, Cmd msg )
interact model player v =
    let
        upsideDown =
            Player.isUpsideDown player

        radius =
            Player.playerRadius

        center =
            Player.getPlayerPosition player

        adjustedVector =
            applyCollision upsideDown radius model center v

        targetSectorCollision =
            center
                |> Point3d.translateBy v
                |> cylinderCollisionSector model upsideDown radius

        adjustedSectorCollision =
            center
                |> Point3d.translateBy adjustedVector
                |> cylinderCollisionSector model upsideDown radius
    in
    case targetSectorCollision of
        Just sector ->
            case ( LevelTile.interact player (getTileAt model sector), adjustedSectorCollision ) of
                ( Just ( tileAfterInteraction, tileCmd ), _ ) ->
                    ( LevelUpdated (updateTile sector tileAfterInteraction model), tileCmd )

                ( _, Just _ ) ->
                    ( LevelCollision Vector3d.zero, Cmd.none )

                _ ->
                    ( LevelCollision adjustedVector, Cmd.none )

        Nothing ->
            ( NoInteraction, Cmd.none )


cylinderCollisionSector : Model -> Bool -> Length.Length -> Point3d Length.Meters WorldCoordinates -> Maybe SectorCoordinates
cylinderCollisionSector level upsideDown radius center =
    cylinderCollisionSectors level upsideDown (Length.inMeters radius) center
        |> List.head


cylinderCollisionSectors : Model -> Bool -> Float -> Point3d Length.Meters WorldCoordinates -> List SectorCoordinates
cylinderCollisionSectors (Level levelData) upsideDown radius center =
    let
        ( centerSectorX, centerSectorY ) =
            Coordinates.worldPositionToSector center

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
        |> List.filter (collisionOnSector (Level levelData) upsideDown)
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


updateTile : SectorCoordinates -> LevelTile.Model -> Model -> Model
updateTile ( x, y ) newTile (Level model) =
    let
        maybePartitionIndex =
            Dict.get y model.sectorYToPartitionIndex

        maybePartition =
            maybePartitionIndex
                |> Maybe.andThen (\partitionIndex -> Array.get partitionIndex model.partitions)
    in
    case ( maybePartitionIndex, maybePartition ) of
        ( Just partitionIndex, Just partition ) ->
            Level { model | partitions = Array.set partitionIndex (Dict.insert ( x, y ) newTile partition) model.partitions }

        _ ->
            Level model


addTrigger : Trigger -> Model -> Model
addTrigger trigger (Level data) =
    case Trigger.narrowedToSector trigger of
        Just sector ->
            Level { data | triggersForSector = Dict.update sector (Maybe.map ((::) trigger) >> Maybe.withDefault [ trigger ] >> Just) data.triggersForSector }

        Nothing ->
            Level { data | globalTriggers = trigger :: data.globalTriggers }


applyCollision : Bool -> Length.Length -> Model -> Point3d Length.Meters WorldCoordinates -> Vector3d Length.Meters WorldCoordinates -> Vector3d Length.Meters WorldCoordinates
applyCollision upsideDown bodyRadius (Level levelData) position v =
    let
        ( cx, cy ) =
            Coordinates.worldPositionToSector position

        desiredPosition =
            translatePointByVector position v

        sectorCollisionsAfterMove =
            cylinderCollisionSectors (Level levelData) upsideDown (Length.inMeters bodyRadius) desiredPosition

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


removeAllTriggersAtSector : SectorCoordinates -> Model -> Model
removeAllTriggersAtSector sector (Level data) =
    Level { data | triggersForSector = Dict.remove sector data.triggersForSector }


removeAllTriggersAtSectors : List SectorCoordinates -> Model -> Model
removeAllTriggersAtSectors sectors (Level data) =
    Level
        { data
            | triggersForSector =
                data.triggersForSector
                    |> Dict.toList
                    |> List.filter (\( sector, _ ) -> not (List.member sector sectors))
                    |> Dict.fromList
        }


removeAllGlobalTriggers : Model -> Model
removeAllGlobalTriggers (Level data) =
    Level
        { data
            | globalTriggers = []
        }


getSignTextAt : Model -> SectorCoordinates -> Maybe String
getSignTextAt level sector =
    getTileAt level sector
        |> LevelTile.getSignText


mapTile : (LevelTile.Model -> LevelTile.Model) -> SectorCoordinates -> Model -> Model
mapTile f sector model =
    let
        previousTile =
            getTileAt model sector

        newTile =
            f previousTile
    in
    updateTile sector newTile model


activateTile : Model -> SectorCoordinates -> ( Model, Cmd msg )
activateTile model sector =
    let
        previousTile =
            getTileAt model sector

        ( newTile, tileCmd ) =
            LevelTile.activate previousTile
    in
    ( updateTile sector newTile model, tileCmd )
