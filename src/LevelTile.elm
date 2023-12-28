module LevelTile exposing
    ( ConfigMsg
    , GroundSound(..)
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
    , customizedSign
    , decoder
    , dependencies
    , editorView
    , empty
    , emptySandbox
    , encode
    , floor
    , getSignText
    , glassWall
    , greenWall
    , groundSound
    , hole
    , interact
    , invisibleWall
    , openFloor
    , redWall
    , sand
    , sandboxWithCastle
    , sign
    , terms
    , toyBucket
    , update
    , updateConfig
    , updateSignText
    , view
    , viewConfig
    , wall
    )

import Angle
import Assets exposing (Dependency)
import Axis3d
import Block3d
import BreakableWall
import Castle
import Color
import Coordinates exposing (ObjectCoordinates, WorldCoordinates)
import Dict
import Frame3d
import Hash exposing (Hash)
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events
import Json.Decode as D
import Json.Encode as E
import Length
import Luminance
import Orientation exposing (Orientation(..))
import Player exposing (Player)
import Point2d
import Point3d exposing (Point3d)
import Scene3d
import Scene3d.Material
import Sound
import Terms
import Vector3d


type Model
    = Floor
    | OpenFloor
    | Wall
    | Sign String String Orientation Model
    | GreenWall
    | RedWall
    | BlueWall
    | Sand
    | ToyBucket
    | InvisibleWall Model
    | BigCastle
    | Chair
    | Sandbox Bool
    | Terms Terms.Model
    | BreakableWall BreakableWallType Orientation BreakableWall.Model
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


greenWall =
    GreenWall


redWall =
    RedWall


sign : String -> Orientation -> Model
sign text orientation =
    customizedSign text orientation Wall


customizedSign : String -> Orientation -> Model -> Model
customizedSign text orientation baseTile =
    let
        name =
            text
                |> Hash.fromString
                |> Hash.toString
    in
    Sign name text orientation baseTile


toyBucket =
    ToyBucket


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


hole =
    Hole


glassWall : Model
glassWall =
    BreakableWall GlassWall South (BreakableWall.init 0.01)


breakableWall : Orientation -> Model
breakableWall orientation =
    BreakableWall HeavyWall orientation (BreakableWall.init 0.05)


isDynamic : Model -> Bool
isDynamic model =
    case model of
        Terms _ ->
            True

        BreakableWall _ _ _ ->
            True

        Sign _ _ _ _ ->
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

        Sign _ _ _ _ ->
            Collision

        BlueWall ->
            Collision

        GreenWall ->
            Collision

        RedWall ->
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

        BreakableWall _ _ breakableWallModel ->
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


wallBlockTile material =
    let
        x1 =
            Length.meters -0.5

        x2 =
            Length.meters 0.5

        y1 =
            Length.meters -0.5

        y2 =
            Length.meters 0.5

        z1 =
            Length.meters 0

        z2 =
            Length.meters 1

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


getCeilingMaterial assets editorMode =
    if editorMode then
        Scene3d.Material.matte (Color.rgba 1 1 1 0.4)

    else
        Scene3d.Material.texturedEmissive
            (Assets.getColorTexture assets "OfficeCeiling005_4K_Color.jpg")
            (Luminance.footLamberts 100)


viewSandbox : Assets.Model -> Bool -> Scene3d.Entity ObjectCoordinates
viewSandbox assets withCastle =
    [ Scene3d.block
        (Scene3d.Material.color Color.darkRed)
        (Block3d.from
            (Point3d.unsafe { x = 0.5, y = -0.5, z = 0 })
            (Point3d.unsafe { x = -0.5, y = -0.45, z = 0.12 })
        )
    , Scene3d.block
        (Scene3d.Material.color Color.darkRed)
        (Block3d.from
            (Point3d.unsafe { x = -0.5, y = -0.5, z = 0 })
            (Point3d.unsafe { x = -0.45, y = 0.5, z = 0.12 })
        )
    , Scene3d.block
        (Scene3d.Material.color Color.darkRed)
        (Block3d.from
            (Point3d.unsafe { x = -0.5, y = 0.45, z = 0 })
            (Point3d.unsafe { x = 0.5, y = 0.5, z = 0.12 })
        )
    , Scene3d.block
        (Scene3d.Material.color Color.darkRed)
        (Block3d.from
            (Point3d.unsafe { x = 0.5, y = -0.5, z = 0 })
            (Point3d.unsafe { x = 0.45, y = 0.5, z = 0.12 })
        )
    , let
        x1 =
            Length.meters 0.45

        y1 =
            Length.meters 0.45

        x2 =
            Length.meters -0.45

        y2 =
            Length.meters -0.45

        z =
            Length.meters 0.08

        sandMaterial =
            Scene3d.Material.texturedNonmetal
                { baseColor = Assets.getColorTexture assets "Ground054_1K-JPG_Color.jpg"
                , roughness = Assets.getOtherTexture assets "Ground054_1K-JPG_Roughness.jpg"
                }
      in
      Scene3d.quad sandMaterial
        (Point3d.xyz x1 y1 z)
        (Point3d.xyz x2 y1 z)
        (Point3d.xyz x2 y2 z)
        (Point3d.xyz x1 y2 z)
    , horizontalTile (Length.meters 1) (getCeilingMaterial assets False)
    ]
        ++ (if withCastle then
                [ Castle.view assets False
                    |> Scene3d.rotateAround Axis3d.z (Angle.degrees 90)
                    |> Scene3d.scaleAbout Point3d.origin 0.05
                    |> Scene3d.placeIn (Frame3d.atPoint (Point3d.meters 0 0 0.08))
                ]

            else
                []
           )
        |> Scene3d.group


view : Assets.Model -> Model -> Scene3d.Entity ObjectCoordinates
view assets model =
    viewInternal assets False model


editorView : Assets.Model -> Model -> Scene3d.Entity ObjectCoordinates
editorView assets model =
    viewInternal assets True model


viewInternal : Assets.Model -> Bool -> Model -> Scene3d.Entity ObjectCoordinates
viewInternal assets editorMode model =
    case model of
        Floor ->
            let
                ceilingMaterial =
                    getCeilingMaterial assets editorMode

                ceilingEntity =
                    horizontalTile (Length.meters 1) ceilingMaterial
            in
            Scene3d.group
                [ viewInternal assets editorMode OpenFloor
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

        GreenWall ->
            let
                material =
                    Scene3d.Material.texturedNonmetal
                        { baseColor = Assets.getColorTexture assets "PaintedPlaster003_4K_Color.jpg"
                        , roughness = Assets.getOtherTexture assets "PaintedPlaster003_4K_Roughness.jpg"
                        }
            in
            wallBlockTile
                material

        RedWall ->
            let
                material =
                    Scene3d.Material.texturedNonmetal
                        { baseColor = Assets.getColorTexture assets "PaintedPlaster001_4K_Color.jpg"
                        , roughness = Assets.getOtherTexture assets "PaintedPlaster001_4K_Roughness.jpg"
                        }
            in
            wallBlockTile
                material

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

        BlackWall ->
            wallBlockTile
                (Scene3d.Material.color Color.black)

        BlackFloor ->
            Scene3d.group
                [ horizontalTile (Length.meters 0) (Scene3d.Material.color Color.black)
                , horizontalTile (Length.meters 1) (Scene3d.Material.color Color.black)
                ]

        Terms termsModel ->
            Scene3d.group [ Terms.view assets termsModel, viewInternal assets editorMode blackFloor ]

        Sign name _ orientation baseTile ->
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
            Scene3d.group [ viewInternal assets editorMode baseTile, signEntity ]

        Sandbox withCastle ->
            viewSandbox assets withCastle

        Sand ->
            let
                material =
                    Scene3d.Material.texturedNonmetal
                        { baseColor = Assets.getColorTexture assets "Ground054_1K-JPG_Color.jpg"
                        , roughness = Assets.getOtherTexture assets "Ground054_1K-JPG_Roughness.jpg"
                        }
            in
            horizontalTile (Length.meters 0) material

        BreakableWall _ orientation breakableWallModel ->
            Scene3d.group
                [ BreakableWall.view assets breakableWallModel
                    |> Scene3d.rotateAround Axis3d.z (Angle.degrees (orientationToRotationAngle orientation))
                , viewInternal assets editorMode floor
                ]

        BigCastle ->
            Scene3d.group
                [ Castle.view assets True
                    |> Scene3d.rotateAround Axis3d.z (Angle.degrees -90)
                    |> Scene3d.scaleAbout Point3d.origin 3
                , viewInternal assets editorMode Sand
                ]

        Hole { walls, barriers } ->
            let
                levels =
                    2

                concreteMaterial =
                    Scene3d.Material.texturedNonmetal
                        { baseColor = Assets.getColorTexture assets "Concrete032_4K_Color.jpg"
                        , roughness = Assets.getOtherTexture assets "Concrete032_4K_Roughness.jpg"
                        }

                concreteWallEntity =
                    Scene3d.quad
                        concreteMaterial
                        (Point3d.meters 0.5 0 0)
                        (Point3d.meters -0.5 0 0)
                        (Point3d.meters -0.5 0 1)
                        (Point3d.meters 0.5 0 1)

                barrierMaterial =
                    Scene3d.Material.texturedMetal
                        { baseColor = Assets.getColorTexture assets "Street_barrier_fence_1_2m_BaseColor.jpg"
                        , roughness = Assets.getOtherTexture assets "Street_barrier_fence_1_2m_Roughness.jpg"
                        }

                barrierEntity =
                    "Street_barrier_fence_1_2m.obj"
                        |> Assets.getMesh assets
                        |> Assets.texturedMesh
                        |> Scene3d.mesh barrierMaterial
                        |> Scene3d.rotateAround Axis3d.x (Angle.degrees 90)
                        |> Scene3d.scaleAbout Point3d.origin 0.43
                        |> Scene3d.placeIn Frame3d.atOrigin

                wallEntities =
                    walls
                        |> List.map
                            (\wallOrientation ->
                                List.range 1 levels
                                    |> List.map
                                        (\level ->
                                            concreteWallEntity
                                                |> Scene3d.translateBy (Vector3d.fromMeters { x = 0, y = 0.5, z = toFloat -level })
                                                |> Scene3d.rotateAround Axis3d.z (Angle.degrees (orientationToRotationAngle wallOrientation))
                                        )
                                    |> Scene3d.group
                            )

                floorEntity =
                    horizontalTile (Length.meters -levels) concreteMaterial

                ceilingEntity =
                    horizontalTile (Length.meters 1) (getCeilingMaterial assets editorMode)

                barrierEntities =
                    barriers
                        |> List.map
                            (\barrierOrientation ->
                                barrierEntity
                                    |> Scene3d.translateBy (Vector3d.fromMeters { x = 0, y = 0.62, z = 0 })
                                    |> Scene3d.rotateAround Axis3d.z (Angle.degrees (orientationToRotationAngle barrierOrientation))
                            )
            in
            Scene3d.group ([ floorEntity, ceilingEntity ] ++ wallEntities ++ barrierEntities)

        ToyBucket ->
            let
                bucketMaterial =
                    Scene3d.Material.texturedMatte (Assets.getColorTexture assets "ToyBucket.png")

                bucketEntity =
                    "ToyBucket.obj"
                        |> Assets.getMesh assets
                        |> Assets.texturedMesh
                        |> Scene3d.mesh bucketMaterial
                        |> Scene3d.placeIn Frame3d.atOrigin
            in
            Scene3d.group
                [ bucketEntity
                    |> Scene3d.scaleAbout Point3d.origin 0.3
                , viewInternal assets editorMode Sand
                ]

        InvisibleWall subModel ->
            viewInternal assets editorMode subModel

        Chair ->
            let
                chairMaterial =
                    Scene3d.Material.texturedMatte (Assets.getColorTexture assets "SofaChairTexture.jpg")
            in
            "Chair.obj"
                |> Assets.getMesh assets
                |> Assets.texturedMesh
                |> Scene3d.mesh chairMaterial
                |> Scene3d.rotateAround Axis3d.z (Angle.degrees 90)
                |> Scene3d.scaleAbout Point3d.origin 0.6
                |> Scene3d.placeIn Frame3d.atOrigin

        Empty ->
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

        GreenWall ->
            [ Assets.ColorTextureDep "PaintedPlaster003_4K_Color.jpg"
            , Assets.OtherTextureDep "PaintedPlaster003_4K_Roughness.jpg"
            ]

        Sign name text _ baseTile ->
            Assets.SignTextureDep name text :: dependencies baseTile

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
            Terms.dependencies

        Sandbox withCastle ->
            [ Assets.ColorTextureDep "Ground054_1K-JPG_Color.jpg"
            , Assets.OtherTextureDep "Ground054_1K-JPG_Roughness.jpg"
            ]
                ++ (if withCastle then
                        Castle.dependencies

                    else
                        []
                   )

        BreakableWall wallType _ _ ->
            Assets.SoundEffectDep
                (case wallType of
                    GlassWall ->
                        "glass-break.mp3"

                    HeavyWall ->
                        "rumble.mp3"
                )
                :: dependencies Wall

        BigCastle ->
            Castle.dependencies

        Chair ->
            [ Assets.ColorTextureDep "SofaChairTexture.jpg"
            , Assets.MeshDep "Chair.obj"
            ]

        Hole { barriers } ->
            [ Assets.ColorTextureDep "Concrete032_4K_Color.jpg"
            , Assets.OtherTextureDep "Concrete032_4K_Roughness.jpg"
            ]
                ++ dependencies openFloor
                ++ (if List.isEmpty barriers then
                        []

                    else
                        [ Assets.ColorTextureDep "Street_barrier_fence_1_2m_BaseColor.jpg"
                        , Assets.OtherTextureDep "Street_barrier_fence_1_2m_Roughness.jpg"
                        , Assets.MeshDep "Street_barrier_fence_1_2m.obj"
                        ]
                   )

        _ ->
            []


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

        BreakableWall _ _ _ ->
            SolidFloor

        _ ->
            VoidGround


update : Float -> Model -> Model
update delta model =
    case model of
        Terms termsModel ->
            Terms (Terms.update delta termsModel)

        BreakableWall wallType orientation breakableWallModel ->
            BreakableWall wallType orientation (BreakableWall.update delta breakableWallModel)

        _ ->
            model


activate : Model -> ( Model, Cmd msg )
activate model =
    case model of
        Terms termsModel ->
            ( Terms (Terms.open termsModel), Sound.playSound "elevator_door.mp3" )

        BreakableWall wallType orientation breakableWallModel ->
            ( BreakableWall wallType orientation (BreakableWall.break (Point2d.meters 0.5 0.5) Vector3d.zero breakableWallModel)
            , Sound.playSound
                (case wallType of
                    GlassWall ->
                        "glass-break.mp3"

                    HeavyWall ->
                        "rumble.mp3"
                )
            )

        _ ->
            ( model, Cmd.none )


interact : Player -> Model -> Maybe ( Model, Cmd msg )
interact player model =
    case model of
        BreakableWall GlassWall orientation breakableWallModel ->
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
                Just ( BreakableWall GlassWall orientation (BreakableWall.break wallCollisionPoint v breakableWallModel), Sound.playSound "glass-break.mp3" )

        _ ->
            Nothing


getSignText : Model -> Maybe String
getSignText model =
    case model of
        Sign _ text _ _ ->
            Just text

        _ ->
            Nothing


updateSignText : String -> Model -> Model
updateSignText newText model =
    case model of
        Sign _ _ orientation baseTile ->
            customizedSign newText orientation baseTile

        _ ->
            model


type alias HoleTileData =
    { walls : List Orientation
    , barriers : List Orientation
    }


type ConfigMsg
    = SelectTileType String
    | SignTextChange String


tilesAvailableForEditor =
    Dict.fromList
        [ ( "Empty", empty )
        , ( "Wall", wall )
        , ( "Floor", floor )
        , ( "Sign", sign "Insert text" Orientation.South )
        ]


updateConfig : ConfigMsg -> Model -> Model
updateConfig msg model =
    case msg of
        SelectTileType tileName ->
            Dict.get tileName tilesAvailableForEditor
                |> Maybe.withDefault model

        SignTextChange newText ->
            case model of
                Sign _ _ orientation baseTile ->
                    customizedSign newText orientation baseTile

                _ ->
                    model


viewConfig : Model -> Html ConfigMsg
viewConfig model =
    Html.div []
        [ Html.select [ Html.Events.onInput SelectTileType ]
            (tilesAvailableForEditor
                |> Dict.keys
                |> List.map (\tileName -> Html.option [ Attr.value tileName ] [ Html.text tileName ])
            )
        , case model of
            Sign name text orientation baseTile ->
                Html.div [ Attr.style "display" "flex", Attr.style "flex-direction" "column" ]
                    [ Html.div [ Attr.style "display" "flex", Attr.style "flex-direction" "row" ]
                        [ Html.span [] [ Html.text "Content:" ]
                        , Html.textarea [ Html.Events.onInput SignTextChange ] [ Html.text text ]
                        ]
                    ]

            _ ->
                Html.div [] []
        ]


decoder : D.Decoder Model
decoder =
    D.oneOf
        [ D.string
            |> D.andThen
                (\str ->
                    case str of
                        "o" ->
                            D.succeed OpenFloor

                        "." ->
                            D.succeed Floor

                        "#" ->
                            D.succeed Wall

                        "#r" ->
                            D.succeed RedWall

                        "#g" ->
                            D.succeed GreenWall

                        "#b" ->
                            D.succeed BlueWall

                        "#0" ->
                            D.succeed BlackWall

                        " " ->
                            D.succeed Empty

                        _ ->
                            D.fail "Invalid tile string"
                )
        , signTileDecoder
        , holeTileDecoder
        , breakableWallTileDecoder
        , invisibleWallTileDecoder
        ]


signTileDecoder =
    D.map4
        (\_ text side base ->
            customizedSign text side base
        )
        (D.field "type" (literalStringDecoder "sign"))
        (D.field "text" D.string)
        (D.field "side" Orientation.decoder)
        (D.field "base" (D.lazy (\() -> decoder)))


holeTileDecoder =
    D.map3
        (\_ walls barriers ->
            hole { walls = walls, barriers = barriers }
        )
        (D.field "type" (literalStringDecoder "hole"))
        (D.field "walls" (D.list Orientation.decoder))
        (D.field "bars" (D.list Orientation.decoder))


breakableWallTileDecoder =
    D.map3
        (\_ subtype orientation ->
            case subtype of
                GlassWall ->
                    glassWall

                HeavyWall ->
                    breakableWall orientation
        )
        (D.field "type" (literalStringDecoder "breakableWall"))
        (D.field "subtype"
            (D.string
                |> D.andThen
                    (\str ->
                        case str of
                            "glass" ->
                                D.succeed GlassWall

                            "heavy" ->
                                D.succeed HeavyWall

                            _ ->
                                D.fail "Invalid breakable wall type"
                    )
            )
        )
        (D.field "side" Orientation.decoder)


invisibleWallTileDecoder =
    D.map2
        (\_ tile -> tile)
        (D.field "type" (literalStringDecoder "invisibleWall"))
        (D.field "tile" (D.lazy (\() -> decoder)))


literalStringDecoder : String -> D.Decoder ()
literalStringDecoder literal =
    D.string
        |> D.andThen
            (\str ->
                if str == literal then
                    D.succeed ()

                else
                    D.fail "Expected string literal"
            )


encode : Model -> E.Value
encode model =
    case model of
        OpenFloor ->
            E.string "o"

        Floor ->
            E.string "."

        Wall ->
            E.string "#"

        Sign _ text orientation baseTile ->
            E.object
                [ ( "type", E.string "sign" )
                , ( "text", E.string text )
                , ( "side", Orientation.encode orientation )
                , ( "base", encode baseTile )
                ]

        Hole { walls, barriers } ->
            E.object
                [ ( "type", E.string "hole" )
                , ( "walls", E.list Orientation.encode walls )
                , ( "bars", E.list Orientation.encode barriers )
                ]

        RedWall ->
            E.string "#r"

        GreenWall ->
            E.string "#g"

        BlueWall ->
            E.string "#b"

        BlackWall ->
            E.string "#0"

        BreakableWall wallType orientation _ ->
            E.object
                [ ( "type", E.string "breakableWall" )
                , ( "subtype"
                  , case wallType of
                        GlassWall ->
                            E.string "glass"

                        HeavyWall ->
                            E.string "heavy"
                  )
                , ( "side", Orientation.encode orientation )
                ]

        InvisibleWall innerTile ->
            E.object
                [ ( "type", E.string "invisibleWall" )
                , ( "tile", encode innerTile )
                ]

        _ ->
            E.string " "



--    = Floor
--    | OpenFloor
--    | Wall
--    | Sign String String Orientation Model
--    | GreenWall
--    | RedWall
--    | BlueWall
--    | Sand
--    | ToyBucket
--    | InvisibleWall Model
--    | BigCastle
--    | Chair
--    | Sandbox Bool
--    | Terms Terms.Model
--    | BreakableWall BreakableWallType Orientation BreakableWall.Model
--    | BlackWall
--    | BlackFloor
--    | Hole HoleTileData
--    | Empty
