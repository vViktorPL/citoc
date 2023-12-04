module LevelTile exposing
    ( GroundSound(..)
    , Model
    , TileCollisionType(..)
    , activate
    , bigCastle
    , blackFloor
    , blackWall
    , blueWall
    , breakableWall
    , chair
    , collision
    , dependencies
    , empty
    , emptySandbox
    , floor
    , glassWall
    , groundSound
    , interact
    , invisibleWall
    , openFloor
    , sand
    , sandboxWithCastle
    , sign
    , terms
    , update
    , view
    , wall
    )

import Angle
import Assets exposing (Dependency)
import Axis3d
import BreakableWall
import Color
import Coordinates exposing (ObjectCoordinates, WorldCoordinates)
import Hash exposing (Hash)
import Length
import Luminance
import Orientation exposing (Orientation(..))
import Player exposing (Player)
import Point2d
import Point3d exposing (Point3d)
import Scene3d
import Scene3d.Material
import Terms
import Vector3d


type Model
    = Floor
    | OpenFloor
    | Wall
    | Sign String String Orientation
    | BlueWall
    | Sand
    | ToyBucket
    | InvisibleWall Model
    | BigCastle
    | Chair
    | Sandbox Bool
    | Terms Terms.Model
    | BreakableWall BreakableWallType BreakableWall.Model
    | BlackWall
    | BlackFloor
    | Hole HoleTileData
    | Empty


type GroundSound
    = SolidFloor
    | SandGround
    | VoidGround


type BreakableWallType
    = GlassWall
    | HeavyWall


floor =
    Floor


openFloor =
    OpenFloor


wall =
    Wall


sign : String -> Orientation -> Model
sign text orientation =
    let
        name =
            text
                |> Hash.fromString
                |> Hash.toString
    in
    Sign name text orientation


blueWall =
    BlueWall


sand =
    Sand


invisibleWall =
    InvisibleWall


sandboxWithCastle =
    Sandbox True


emptySandbox =
    Sandbox False


terms =
    Terms Terms.init


blackWall =
    BlackWall


empty =
    Empty


chair =
    Chair


bigCastle =
    BigCastle


blackFloor =
    BlackFloor


glassWall : Model
glassWall =
    BreakableWall GlassWall (BreakableWall.init 0.01)


breakableWall : Model
breakableWall =
    BreakableWall HeavyWall (BreakableWall.init 0.05)


isDynamic : Model -> Bool
isDynamic model =
    case model of
        Terms _ ->
            True

        BreakableWall _ _ ->
            True

        Sign _ _ _ ->
            True

        _ ->
            False


isStatic : Model -> Bool
isStatic =
    isDynamic >> not


type TileCollisionType
    = NoCollision
    | Collision
    | FloorCollision


collision : Model -> TileCollisionType
collision model =
    case model of
        Wall ->
            Collision

        Sign _ _ _ ->
            Collision

        BlueWall ->
            Collision

        BlackWall ->
            Collision

        InvisibleWall _ ->
            Collision

        Hole _ ->
            FloorCollision

        Sandbox _ ->
            Collision

        Terms termsModel ->
            case Terms.doesCollide termsModel of
                True ->
                    Collision

                False ->
                    NoCollision

        BreakableWall _ breakableWallModel ->
            case BreakableWall.isBroken breakableWallModel of
                True ->
                    NoCollision

                False ->
                    Collision

        _ ->
            NoCollision


horizontalTile z material =
    let
        x1 =
            Length.meters 0.5

        y1 =
            Length.meters 0.5

        x2 =
            Length.meters -0.5

        y2 =
            Length.meters -0.5
    in
    Scene3d.quad material
        (Point3d.xyz x1 y1 z)
        (Point3d.xyz x2 y1 z)
        (Point3d.xyz x2 y2 z)
        (Point3d.xyz x1 y2 z)


wallBlockTile material { x1, x2, y1, y2, z1, z2 } =
    let
        leftQuad =
            Scene3d.quad material
                (Point3d.xyz x1 y1 z1)
                (Point3d.xyz x1 y2 z1)
                (Point3d.xyz x1 y2 z2)
                (Point3d.xyz x1 y1 z2)

        frontQuad =
            Scene3d.quad material
                (Point3d.xyz x1 y2 z1)
                (Point3d.xyz x2 y2 z1)
                (Point3d.xyz x2 y2 z2)
                (Point3d.xyz x1 y2 z2)

        behindQuad =
            Scene3d.quad material
                (Point3d.xyz x1 y1 z1)
                (Point3d.xyz x2 y1 z1)
                (Point3d.xyz x2 y1 z2)
                (Point3d.xyz x1 y1 z2)

        rightQuad =
            Scene3d.quad material
                (Point3d.xyz x2 y2 z1)
                (Point3d.xyz x2 y1 z1)
                (Point3d.xyz x2 y1 z2)
                (Point3d.xyz x2 y2 z2)
    in
    Scene3d.group [ leftQuad, frontQuad, behindQuad, rightQuad ]


view : Assets.Model -> Model -> Scene3d.Entity ObjectCoordinates
view assets model =
    case model of
        Floor ->
            let
                ceilingMaterial =
                    Scene3d.Material.texturedEmissive
                        (Assets.getColorTexture assets "OfficeCeiling005_4K_Color.jpg")
                        (Luminance.footLamberts 100)

                ceilingEntity =
                    horizontalTile (Length.meters 1) ceilingMaterial
            in
            Scene3d.group
                [ view assets OpenFloor
                , ceilingEntity
                ]

        OpenFloor ->
            let
                material =
                    Scene3d.Material.texturedNonmetal
                        { baseColor = Assets.getColorTexture assets "CheckerFloor.jpg"
                        , roughness = Scene3d.Material.constant 0.5
                        }
            in
            horizontalTile (Length.meters 0) material

        Wall ->
            let
                material =
                    Scene3d.Material.texturedNonmetal
                        { baseColor = Assets.getColorTexture assets "Bricks021_1K-JPG_Color.jpg"
                        , roughness = Assets.getOtherTexture assets "Bricks021_1K-JPG_Roughness.jpg"
                        }
            in
            wallBlockTile
                material
                { x1 = Length.meters -0.5
                , x2 = Length.meters 0.5
                , y1 = Length.meters -0.5
                , y2 = Length.meters 0.5
                , z1 = Length.meters 0
                , z2 = Length.meters 1
                }

        BlueWall ->
            let
                material =
                    Scene3d.Material.texturedPbr
                        { baseColor = Assets.getColorTexture assets "CorrugatedSteel007B_1K-JPG_Color.jpg"
                        , metallic = Assets.getOtherTexture assets "CorrugatedSteel007B_1K-JPG_Metalness.jpg"
                        , roughness = Assets.getOtherTexture assets "CorrugatedSteel007B_1K-JPG_Roughness.jpg"
                        }
            in
            wallBlockTile
                material
                { x1 = Length.meters -0.5
                , x2 = Length.meters 0.5
                , y1 = Length.meters -0.5
                , y2 = Length.meters 0.5
                , z1 = Length.meters 0
                , z2 = Length.meters 1
                }

        BlackWall ->
            wallBlockTile
                (Scene3d.Material.color Color.black)
                { x1 = Length.meters -0.5
                , x2 = Length.meters 0.5
                , y1 = Length.meters -0.5
                , y2 = Length.meters 0.5
                , z1 = Length.meters 0
                , z2 = Length.meters 1
                }

        BlackFloor ->
            Scene3d.group
                [ horizontalTile (Length.meters 0) (Scene3d.Material.color Color.black)
                , horizontalTile (Length.meters 1) (Scene3d.Material.color Color.black)
                ]

        Terms termsModel ->
            Scene3d.group [ Terms.view assets termsModel, view assets blackFloor ]

        Sign name _ orientation ->
            let
                material =
                    name
                        |> Assets.getSignTexture assets
                        |> Scene3d.Material.texturedMatte

                xMargin =
                    0.2

                topMargin =
                    0.2

                bottomMargin =
                    0.5

                x1 =
                    Length.meters (0.5 - xMargin)

                x2 =
                    Length.meters (-0.5 + xMargin)

                y =
                    Length.meters 0.51

                z1 =
                    Length.meters bottomMargin

                z2 =
                    Length.meters (1 - topMargin)

                signEntity =
                    Scene3d.quad material
                        (Point3d.xyz x1 y z1)
                        (Point3d.xyz x2 y z1)
                        (Point3d.xyz x2 y z2)
                        (Point3d.xyz x1 y z2)
                        |> Scene3d.rotateAround Axis3d.z (Angle.degrees (orientationToRotationAngle orientation))
            in
            Scene3d.group [ view assets Wall, signEntity ]

        Sand ->
            let
                material =
                    Scene3d.Material.texturedNonmetal
                        { baseColor = Assets.getColorTexture assets "Ground054_1K-JPG_Color.jpg"
                        , roughness = Assets.getOtherTexture assets "Ground054_1K-JPG_Roughness.jpg"
                        }
            in
            horizontalTile (Length.meters 0) material

        BreakableWall _ breakableWallModel ->
            BreakableWall.view assets breakableWallModel

        -- TODO: rest tiles
        _ ->
            Scene3d.nothing


orientationToRotationAngle orientation =
    case orientation of
        South ->
            0

        West ->
            -90

        North ->
            180

        East ->
            90


dependencies : Model -> List Dependency
dependencies model =
    case model of
        Floor ->
            Assets.ColorTextureDep "OfficeCeiling005_4K_Color.jpg" :: dependencies OpenFloor

        OpenFloor ->
            [ Assets.ColorTextureDep "CheckerFloor.jpg"
            , Assets.SoundEffectDep "step-1.mp3"
            , Assets.SoundEffectDep "step-2.mp3"
            , Assets.SoundEffectDep "step-3.mp3"
            , Assets.SoundEffectDep "step-4.mp3"
            , Assets.SoundEffectDep "step-5.mp3"
            ]

        Wall ->
            [ Assets.ColorTextureDep "Bricks021_1K-JPG_Color.jpg"
            , Assets.OtherTextureDep "Bricks021_1K-JPG_Roughness.jpg"
            ]

        Sign name text _ ->
            Assets.SignTextureDep name text :: dependencies Wall

        BlueWall ->
            [ Assets.ColorTextureDep "CorrugatedSteel007B_1K-JPG_Color.jpg"
            , Assets.OtherTextureDep "CorrugatedSteel007B_1K-JPG_Metalness.jpg"
            , Assets.OtherTextureDep "CorrugatedSteel007B_1K-JPG_Roughness.jpg"
            ]

        Sand ->
            [ Assets.ColorTextureDep "Ground054_1K-JPG_Color.jpg"
            , Assets.OtherTextureDep "Ground054_1K-JPG_Roughness.jpg"
            , Assets.SoundEffectDep "sand-step.mp3"
            ]

        ToyBucket ->
            [ Assets.ColorTextureDep "ToyBucket.png"
            , Assets.MeshDep "ToyBucket.obj"
            ]
                ++ dependencies Sand

        InvisibleWall tile ->
            dependencies tile

        Terms _ ->
            [ Assets.ColorTextureDep "toc.png" ]

        --BreakableWall _ ->
        --    [ Assets.SoundEffectDep "glass-break.mp3"
        --    ]
        --        ++ dependencies Wall
        _ ->
            []



--type LevelInteractionResult
--    = NoInteraction
--    | LevelCollision (Vector3d Length.Meters WorldCoordinates)
--    | LevelUpdated Level
-- TODO: need this?
--type TileInteractionResult
--    = NoInteraction
--    | Collision
--    | TileUpdated Model
--interact : Player -> Point3d Length.Meters WorldCoordinates -> Model -> TileInteractionResult
--interact player modelWorldPosition model =
--    Player.getPlayerPosition player


groundSound : Model -> GroundSound
groundSound model =
    case model of
        OpenFloor ->
            SolidFloor

        Floor ->
            SolidFloor

        BlackFloor ->
            SolidFloor

        Terms _ ->
            SolidFloor

        Sand ->
            SandGround

        ToyBucket ->
            SandGround

        Wall ->
            SolidFloor

        BreakableWall _ _ ->
            SolidFloor

        _ ->
            VoidGround


update : Float -> Model -> Model
update delta model =
    case model of
        Terms termsModel ->
            Terms (Terms.update delta termsModel)

        BreakableWall wallType breakableWallModel ->
            BreakableWall wallType (BreakableWall.update delta breakableWallModel)

        _ ->
            model


activate : Model -> Model
activate model =
    case model of
        Terms termsModel ->
            Terms (Terms.open termsModel)

        BreakableWall wallType breakableWallModel ->
            BreakableWall wallType (BreakableWall.break (Point2d.meters 0.5 0.5) Vector3d.zero breakableWallModel)

        _ ->
            model


interact : Player -> Model -> Maybe Model
interact player model =
    case model of
        BreakableWall GlassWall breakableWallModel ->
            if BreakableWall.isBroken breakableWallModel then
                Nothing

            else
                let
                    v =
                        Player.getMovementVector player

                    playerPosition =
                        Player.getPlayerPosition player

                    wallCollisionPointX =
                        Coordinates.worldPositionToSectorOffsetX playerPosition

                    wallCollisionPointY =
                        Point3d.zCoordinate playerPosition
                            |> Length.inMeters

                    wallCollisionPoint =
                        Point2d.meters wallCollisionPointX wallCollisionPointY
                in
                Just (BreakableWall GlassWall (BreakableWall.break wallCollisionPoint v breakableWallModel))

        _ ->
            Nothing



--BigCastle ->
--Chair ->


type alias HoleTileData =
    { walls : List Orientation
    , barriers : List Orientation
    }
