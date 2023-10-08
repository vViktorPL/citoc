module Level exposing (..)

import Point3d
import Length
import Array exposing (Array)
import Scene3d
import Scene3d.Material
import Textures exposing (Model, TextureToLoad)
import Luminance
import Vector3d exposing (Vector3d)
import Point3d exposing (Point3d)
import Quantity exposing (Quantity)

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
    | Sign String Orientation
    | BlueWall
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

createTexturedBlock material { x1, x2, y1, y2, z1, z2 } =
    let
        leftQuad = Scene3d.quad material
            (Point3d.xyz x1 y1 z1)
            (Point3d.xyz x1 y2 z1)
            (Point3d.xyz x1 y2 z2)
            (Point3d.xyz x1 y1 z2)

        frontQuad = Scene3d.quad material
            (Point3d.xyz x1 y2 z1)
            (Point3d.xyz x2 y2 z1)
            (Point3d.xyz x2 y2 z2)
            (Point3d.xyz x1 y2 z2)

        behindQuad = Scene3d.quad material
            (Point3d.xyz x1 y1 z1)
            (Point3d.xyz x2 y1 z1)
            (Point3d.xyz x2 y1 z2)
            (Point3d.xyz x1 y1 z2)

        rightQuad = Scene3d.quad material
            (Point3d.xyz x2 y2 z1)
            (Point3d.xyz x2 y1 z1)
            (Point3d.xyz x2 y1 z2)
            (Point3d.xyz x2 y2 z2)
    in
        Scene3d.group [leftQuad, frontQuad, behindQuad, rightQuad]

viewFloor textures (x, y) =
   Maybe.map2
        (\floorTexture ceilingTexture ->
            let
               floorMaterial = (Scene3d.Material.texturedNonmetal { baseColor = floorTexture, roughness = Scene3d.Material.constant 0.5 })
               ceilingMaterial = (Scene3d.Material.texturedEmissive ceilingTexture (Luminance.footLamberts 100 ))
               x1 = Length.meters (toFloat -x)
               y1 = Length.meters (toFloat (y + 1))
               x2 = Length.meters (toFloat -x - 1)
               y2 = Length.meters (toFloat (y + 1))
               x3 = Length.meters (toFloat -x - 1)
               y3 = Length.meters (toFloat y)
               x4 = Length.meters (toFloat -x)
               y4 = Length.meters (toFloat y)

               zBottom = Length.meters 0
               zTop = Length.meters 1
            in
                Scene3d.group
                    [ Scene3d.quad floorMaterial
                         (Point3d.xyz x1 y1 zBottom)
                         (Point3d.xyz x2 y2 zBottom)
                         (Point3d.xyz x3 y3 zBottom)
                         (Point3d.xyz x4 y4 zBottom)
                    , Scene3d.quad ceilingMaterial
                         (Point3d.xyz x1 y1 zTop)
                         (Point3d.xyz x2 y2 zTop)
                         (Point3d.xyz x3 y3 zTop)
                         (Point3d.xyz x4 y4 zTop)
                    ]


        )
        --(Textures.getTexture textures "CheckerFloor.jpg")
        (Textures.getTexture textures "Ground054_1K-JPG_Color.jpg")
        (Textures.getTexture textures "OfficeCeiling005_4K_Color.jpg")
        |> Maybe.withDefault Scene3d.nothing

viewBlock textures (x, y) =
    Maybe.map2
        (\texture roughness ->
            (createTexturedBlock
                (Scene3d.Material.texturedNonmetal { baseColor = texture, roughness = roughness })
                { x1 = Length.meters (toFloat -x)
                , x2 = Length.meters (toFloat -(x + 1) )
                , y1 = Length.meters (toFloat y)
                , y2 = Length.meters (toFloat (y + 1))
                , z1 = Length.meters 0
                , z2 = Length.meters 1
                }
            )
        )
        (Textures.getTexture textures "Bricks021_1K-JPG_Color.jpg")
        (Textures.getTextureFloat textures "Bricks021_1K-JPG_Roughness.jpg")
        |> Maybe.withDefault Scene3d.nothing


viewSign : Model -> String -> Orientation -> (Int, Int) -> Scene3d.Entity WorldCoordinates
viewSign textures texture orientation (x, y) =
    Textures.getTexture textures texture
          |> Maybe.map
              (\signTexture ->
                    let
                        material = Scene3d.Material.texturedMatte signTexture
                        x1 = Length.meters (toFloat -x)
                        x2 = Length.meters (toFloat -(x + 1) )
                        y1 = Length.meters (toFloat y)
                        y2 = Length.meters (toFloat (y + 1))
                        z1 = Length.meters 0
                        z2 = Length.meters 1

                        xMargin = 0.2
                        topMargin = 0.2
                        bottomMargin = 0.5
                        bumpMargin = 0.01
                    in
                    case orientation of
                        North ->
                            (Scene3d.quad material
                               (Point3d.xyz
                                  (Length.meters ((toFloat -(x + 1) ) + xMargin))
                                  (Length.meters ((toFloat y) - bumpMargin))
                                  (Length.meters bottomMargin)
                               )
                               (Point3d.xyz
                                    (Length.meters ((toFloat -x) - xMargin))
                                    (Length.meters ((toFloat y) - bumpMargin))
                                    (Length.meters bottomMargin)
                                )
                               (Point3d.xyz
                                    (Length.meters ((toFloat -x) - xMargin))
                                    (Length.meters ((toFloat y) - bumpMargin))
                                    (Length.meters (1 - topMargin))
                                )
                               (Point3d.xyz
                                    (Length.meters ((toFloat -(x + 1) ) + xMargin))
                                    (Length.meters ((toFloat y) - bumpMargin))
                                    (Length.meters (1 - topMargin))
                                )
                            )
                        South ->
                           (Scene3d.quad material
                               (Point3d.xyz
                                  (Length.meters ((toFloat -x) - xMargin))
                                  (Length.meters ((toFloat (y + 1)) + bumpMargin))
                                  (Length.meters bottomMargin)
                               )
                               (Point3d.xyz
                                    (Length.meters ((toFloat -(x + 1) ) + xMargin))
                                    (Length.meters ((toFloat (y + 1)) + bumpMargin))
                                    (Length.meters bottomMargin)
                                )
                               (Point3d.xyz
                                    (Length.meters ((toFloat -(x + 1) ) + xMargin))
                                    (Length.meters ((toFloat (y + 1)) + bumpMargin))
                                    (Length.meters (1 - topMargin))
                                )
                               (Point3d.xyz
                                    (Length.meters ((toFloat -x) - xMargin))
                                    (Length.meters ((toFloat (y + 1)) + bumpMargin))
                                    (Length.meters (1 - topMargin))
                                )
                           )
                        East ->
                               --(Point3d.xyz x1 y1 z1)
                               --         (Point3d.xyz x1 y2 z1)
                               --         (Point3d.xyz x1 y2 z2)
                               --         (Point3d.xyz x1 y1 z2)
                            (Scene3d.quad material
                               (Point3d.xyz
                                  (Length.meters ((toFloat -(x + 1) ) - bumpMargin))
                                  (Length.meters ((toFloat (y + 1)) - xMargin))
                                  (Length.meters bottomMargin)
                               )
                               (Point3d.xyz
                                    (Length.meters ((toFloat -(x + 1) ) - bumpMargin))
                                    (Length.meters ((toFloat y) + xMargin))
                                    (Length.meters bottomMargin)
                               )
                               (Point3d.xyz
                                     (Length.meters ((toFloat -(x + 1) ) - bumpMargin))
                                     (Length.meters ((toFloat y) + xMargin))
                                     (Length.meters (1 - topMargin))
                               )
                               (Point3d.xyz
                                     (Length.meters ((toFloat -(x + 1) ) - bumpMargin))
                                     (Length.meters ((toFloat (y + 1)) - xMargin))
                                     (Length.meters (1 - topMargin))
                               )
                            )
                        West ->
                               --(Point3d.xyz x1 y1 z1)
                               --         (Point3d.xyz x1 y2 z1)
                               --         (Point3d.xyz x1 y2 z2)
                               --         (Point3d.xyz x1 y1 z2)
                            (Scene3d.quad material
                               (Point3d.xyz
                                  (Length.meters ((toFloat -x) + bumpMargin))
                                  (Length.meters ((toFloat y) + xMargin))
                                  (Length.meters bottomMargin)
                               )
                               (Point3d.xyz
                                    (Length.meters ((toFloat -x) + bumpMargin))
                                    (Length.meters ((toFloat (y + 1)) - xMargin))
                                    (Length.meters bottomMargin)
                               )
                               (Point3d.xyz
                                     (Length.meters ((toFloat -x) + bumpMargin))
                                     (Length.meters ((toFloat (y + 1)) - xMargin))
                                     (Length.meters (1 - topMargin))
                               )
                               (Point3d.xyz
                                     (Length.meters ((toFloat -x) + bumpMargin))
                                     (Length.meters ((toFloat y) + xMargin))
                                     (Length.meters (1 - topMargin))
                               )
                            )
              )
          |> Maybe.withDefault Scene3d.nothing


view : Model -> Level -> Scene3d.Entity WorldCoordinates
view textures (Level levelData) =
    levelData.tiles
        |> Array.indexedMap
            (\y row ->
                Array.indexedMap
                    (\x tile ->
                        case tile of
                            Wall ->
                                viewBlock textures (x, y)

                            Sign texture orientation ->
                                Scene3d.group
                                    [ viewBlock textures (x, y)
                                    , viewSign textures texture orientation (x, y)
                                    ]

                            BlueWall ->
                                Maybe.map3
                                    (\texture metalness roughness ->
                                        createTexturedBlock
                                            (Scene3d.Material.texturedPbr { baseColor = texture, metallic = metalness, roughness = roughness } )
                                            { x1 = Length.meters (toFloat -x)
                                            , x2 = Length.meters (toFloat -(x + 1) )
                                            , y1 = Length.meters (toFloat y)
                                            , y2 = Length.meters (toFloat (y + 1))
                                            , z1 = Length.meters 0
                                            , z2 = Length.meters 1
                                            }
                                    )
                                    (Textures.getTexture textures "CorrugatedSteel007B_1K-JPG_Color.jpg")
                                    (Textures.getTextureFloat textures "CorrugatedSteel007B_1K-JPG_Metalness.jpg")
                                    (Textures.getTextureFloat textures "CorrugatedSteel007B_1K-JPG_Roughness.jpg")
                                    |> Maybe.withDefault Scene3d.nothing
                            Floor ->
                                viewFloor textures (x, y)

                            _ -> Scene3d.nothing
                    )
                    row
                    |> Array.toList
            )
        |> Array.toList
        |> List.concatMap identity
        |> Scene3d.group