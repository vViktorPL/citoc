module SceneAssets exposing
    (Model
    , Msg
    , init
    , update
    , subscription
    , ready
    , wallBlock
    , blueWallBlock
    , floorTile
    , ceilingTile
    , sign
    , sandTile
    , toyBucket
    , castleWall
    , castleDoor
    , castleWallTower
    , castleEntryVoid
    , chair
    , sandbox
    )

import Textures exposing (TextureToLoad(..), TexturesState(..))
import MeshCollection exposing (Model(..))
import Scene3d
import Scene3d.Material
import Length exposing (Length, Meters)
import Obj.Decode exposing (ObjCoordinates)
import Dict exposing (Dict)
import Point3d
import Luminance
import Color
import Axis3d
import Angle
import Block3d

type alias SceneEntity = Scene3d.Entity ObjCoordinates

type alias LoadingAssetsData =
     { textures: Textures.Model
     , meshes: MeshCollection.Model
     }

type alias ReadyAssetsData =
    { wallBlock: SceneEntity
    , blueWallBlock: SceneEntity
    , floorTile: SceneEntity
    , ceilingTile: SceneEntity
    , signs: Dict String SceneEntity
    , sandTile: SceneEntity
    , toyBucket: SceneEntity
    , castleWall : SceneEntity
    , castleDoor: SceneEntity
    , castleWallTower : SceneEntity
    , castleEntryVoid : SceneEntity
    , chair : SceneEntity
    , sandbox : SceneEntity
    }

type Model
    = LoadingAssets LoadingAssetsData
    | ReadyAssets ReadyAssetsData


type Msg
    = TexturesMsg Textures.Msg
    | MeshesMsg MeshCollection.Msg


texturesToLoad =
    [ TextureColor "Bricks021_1K-JPG_Color.jpg"
    , TextureFloat "Bricks021_1K-JPG_Roughness.jpg"
    , TextureColor "CheckerFloor.jpg"
    , TextureColor "OfficeCeiling005_4K_Color.jpg"
    , TextureColor "CorrugatedSteel007B_1K-JPG_Color.jpg"
    , TextureFloat "CorrugatedSteel007B_1K-JPG_Metalness.jpg"
    , TextureFloat "CorrugatedSteel007B_1K-JPG_Roughness.jpg"
    , GenerateSign "Sign-ConfusingCorridor" "BEWARE OF\nCONFUSING\nCORRIDORS"
    , GenerateSign "Sign-Quiz" "QUIZ TIME!\n\nDo you think that there is a\ndead end around the corner?"
    , GenerateSign "Sign-CorrectAnswer" "Correct answer!\n\nIt seems that you were right!"
    , GenerateSign "Sign-CountTo3" "Let's count to 3"
    , GenerateSign "Sign-1" "1"
    , GenerateSign "Sign-2" "2"
    , GenerateSign "Sign-3" "3"
    , GenerateSign "Sign-ProgrammerZero" "Actually,\na good programmer\nwould start with zero...\nso let's start over..."
    , GenerateSign "Sign-JustKiddin" "Just kidding"
    , GenerateSign "Sign-Moonwalk" "MOONWALKERS\nONLY"
    , GenerateSign "Sign-Minus1" "-1"
    , TextureColor "ConeColor.jpg"
    , TextureColor "Ground054_1K-JPG_Color.jpg"
    , TextureFloat "Ground054_1K-JPG_Roughness.jpg"
    , TextureColor "ToyBucket.png"
    , TextureColor "SofaChairTexture.jpg"
    ]

meshesToLoad =
    [ "ToyBucket.obj"
    , "wall.obj"
    --, "wallCorner.obj"
    , "wallDoor.obj"
    , "wallCornerHalfTower.obj"
    , "Chair.obj"
    ]

init : (Model, Cmd Msg)
init =
    let
        (textures, texturesCmd) = Textures.init texturesToLoad
        (meshes, meshesCmd) = MeshCollection.init meshesToLoad
    in
    ( LoadingAssets
        { textures = textures
        , meshes = meshes
        }
    , Cmd.batch
        [ Cmd.map TexturesMsg texturesCmd
        , Cmd.map MeshesMsg meshesCmd
        ]
    )


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    let
        (updatedModel, cmd) =
            case (msg, model) of
                (TexturesMsg texturesMsg, LoadingAssets assetsData) ->
                    let
                        (newTextures, texturesCmd) = Textures.update texturesMsg assetsData.textures
                    in
                    (LoadingAssets { assetsData | textures = newTextures }, Cmd.map TexturesMsg texturesCmd)

                (MeshesMsg meshesMsg, LoadingAssets assetsData) ->
                    let
                        (newMeshes, meshesCmd) = MeshCollection.update meshesMsg assetsData.meshes
                    in
                    (LoadingAssets { assetsData | meshes = newMeshes }, Cmd.map MeshesMsg meshesCmd)

                _ -> (model, Cmd.none)
    in
        (initializeEntities updatedModel, cmd)

ready : Model -> Bool
ready model =
    case model of
        ReadyAssets _ -> True
        _ -> False

initializeEntities : Model -> Model
initializeEntities model =
    case model of
        (ReadyAssets _) -> model
        (LoadingAssets { textures, meshes }) ->
            (case (Textures.getState textures, meshes) of
                 (TexturesLoaded, MeshCollectionLoaded _) ->
                     ReadyAssets
                        { wallBlock =
                            Maybe.map2
                                (\texture roughness ->
                                    createTexturedBlock
                                        (Scene3d.Material.texturedNonmetal { baseColor = texture, roughness = roughness })
                                        { x1 = Length.meters -0.5
                                        , x2 = Length.meters 0.5
                                        , y1 = Length.meters -0.5
                                        , y2 = Length.meters 0.5
                                        , z1 = Length.meters 0
                                        , z2 = Length.meters 1
                                        }
                                )
                                (Textures.getTexture textures "Bricks021_1K-JPG_Color.jpg")
                                (Textures.getTextureFloat textures "Bricks021_1K-JPG_Roughness.jpg")
                            |> Maybe.withDefault Scene3d.nothing

                        , blueWallBlock =
                            Maybe.map3
                                    (\texture metalness roughness ->
                                        createTexturedBlock
                                            (Scene3d.Material.texturedPbr { baseColor = texture, metallic = metalness, roughness = roughness } )
                                             { x1 = Length.meters -0.5
                                             , x2 = Length.meters 0.5
                                             , y1 = Length.meters -0.5
                                             , y2 = Length.meters 0.5
                                             , z1 = Length.meters 0
                                             , z2 = Length.meters 1
                                             }
                                    )
                                    (Textures.getTexture textures "CorrugatedSteel007B_1K-JPG_Color.jpg")
                                    (Textures.getTextureFloat textures "CorrugatedSteel007B_1K-JPG_Metalness.jpg")
                                    (Textures.getTextureFloat textures "CorrugatedSteel007B_1K-JPG_Roughness.jpg")
                                    |> Maybe.withDefault Scene3d.nothing

                        , floorTile =
                            Maybe.map
                                (\floorTexture ->
                                    createTexturedFloor
                                        (Length.meters 0)
                                        (Scene3d.Material.texturedNonmetal { baseColor = floorTexture, roughness = Scene3d.Material.constant 0.5 })
                                )
                                (Textures.getTexture textures "CheckerFloor.jpg")
                                |> Maybe.withDefault Scene3d.nothing
                        , ceilingTile =
                             Maybe.map
                                (\ceilingTexture ->
                                    createTexturedFloor
                                        (Length.meters 1)
                                        (Scene3d.Material.texturedEmissive ceilingTexture (Luminance.footLamberts 100 ))
                                )
                                (Textures.getTexture textures "OfficeCeiling005_4K_Color.jpg")
                                |> Maybe.withDefault Scene3d.nothing
                        , signs =
                            signTextureIds
                                |> List.map
                                    (\textureName ->
                                        Maybe.map
                                            (\texture ->
                                                let
                                                    material = Scene3d.Material.texturedMatte texture
                                                in
                                                    (textureName, createSignQuad material)
                                            )
                                            (Textures.getTexture textures textureName)
                                        |> Maybe.withDefault (textureName, Scene3d.nothing)
                                    )
                                |> Dict.fromList
                        , sandTile =
                            (Maybe.map2
                                (\sandTexture roughnessTexture ->
                                    createTexturedFloor
                                        (Length.meters 0)
                                        (Scene3d.Material.texturedNonmetal { baseColor = sandTexture, roughness = roughnessTexture })
                                )
                                (Textures.getTexture textures "Ground054_1K-JPG_Color.jpg")
                                (Textures.getTextureFloat textures "Ground054_1K-JPG_Roughness.jpg")
                            )
                                |> Maybe.withDefault Scene3d.nothing
                        , toyBucket =
                            (MeshCollection.getMeshEntity meshes "ToyBucket.obj" (Textures.getTexture textures "ToyBucket.png"))
                                 |> Maybe.map (Scene3d.scaleAbout (Point3d.meters 0 0 0) 0.3)
                                 |> Maybe.withDefault Scene3d.nothing
                        , castleWall =
                            (MeshCollection.getMeshEntity meshes "wall.obj" (Textures.getTexture textures "Ground054_1K-JPG_Color.jpg"))
                            |> Maybe.withDefault Scene3d.nothing
                        , castleDoor =
                            (MeshCollection.getMeshEntity meshes "wallDoor.obj" (Textures.getTexture textures "Ground054_1K-JPG_Color.jpg"))
                            |> Maybe.withDefault Scene3d.nothing
                        , castleWallTower =
                            (MeshCollection.getMeshEntity meshes "wallCornerHalfTower.obj" (Textures.getTexture textures "Ground054_1K-JPG_Color.jpg"))
                                |> Maybe.withDefault Scene3d.nothing
                        , castleEntryVoid =
                            Scene3d.quad
                                (Scene3d.Material.color Color.black)
                                (Point3d.unsafe { x = 0, y = 0, z = 0 })
                                (Point3d.unsafe { x = 0, y = -1, z = 0 })
                                (Point3d.unsafe { x = 0, y = -1, z = 1 })
                                (Point3d.unsafe { x = 0, y = 0, z = 1 })
                        , chair =
                            (MeshCollection.getMeshEntity meshes "Chair.obj" (Textures.getTexture textures "SofaChairTexture.jpg"))
                                |> Maybe.map (Scene3d.rotateAround (Axis3d.z) (Angle.degrees 90))
                                |> Maybe.map (Scene3d.scaleAbout Point3d.origin 0.6)
                                |> Maybe.withDefault Scene3d.nothing
                        , sandbox =
                            Scene3d.group
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
                                ,
                                    let
                                          x1 = Length.meters 0.45
                                          y1 = Length.meters 0.45
                                          x2 = Length.meters -0.45
                                          y2 = Length.meters -0.45
                                          z = Length.meters 0.08
                                    in
                                      Maybe.map2
                                        (\sandTexture roughnessTexture ->
                                          Scene3d.quad (Scene3d.Material.texturedNonmetal { baseColor = sandTexture, roughness = roughnessTexture })
                                               (Point3d.xyz x1 y1 z)
                                               (Point3d.xyz x2 y1 z)
                                               (Point3d.xyz x2 y2 z)
                                               (Point3d.xyz x1 y2 z)
                                        )
                                        (Textures.getTexture textures "Ground054_1K-JPG_Color.jpg")
                                        (Textures.getTextureFloat textures "Ground054_1K-JPG_Roughness.jpg")
                                      |> Maybe.withDefault Scene3d.nothing
                                ]
                        }

                 _ -> model
            )


signTextureIds = List.filterMap
    (\textureToLoad -> case textureToLoad of
        GenerateSign signId _ ->
            Just signId

        _ -> Nothing
    ) texturesToLoad


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


createSkybox { x1, x2, y1, y2, z1, z2 } matLeft matFront matRight  =
     let
            leftQuad = Scene3d.quad matLeft
                (Point3d.xyz x1 y1 z1)
                (Point3d.xyz x1 y2 z1)
                (Point3d.xyz x1 y2 z2)
                (Point3d.xyz x1 y1 z2)

            frontQuad = Scene3d.quad matFront
                (Point3d.xyz x1 y2 z1)
                (Point3d.xyz x2 y2 z1)
                (Point3d.xyz x2 y2 z2)
                (Point3d.xyz x1 y2 z2)

            rightQuad = Scene3d.quad matRight
                (Point3d.xyz x2 y2 z1)
                (Point3d.xyz x2 y1 z1)
                (Point3d.xyz x2 y1 z2)
                (Point3d.xyz x2 y2 z2)
        in
            Scene3d.group [leftQuad, frontQuad, rightQuad]

createTexturedFloor z material =
    let
        x1 = Length.meters 0.5
        y1 = Length.meters 0.5
        x2 = Length.meters -0.5
        y2 = Length.meters -0.5
    in
    Scene3d.quad material
         (Point3d.xyz x1 y1 z)
         (Point3d.xyz x2 y1 z)
         (Point3d.xyz x2 y2 z)
         (Point3d.xyz x1 y2 z)

createSignQuad material =
     let
        xMargin = 0.2
        topMargin = 0.2
        bottomMargin = 0.5

        x1 = Length.meters (0.5 - xMargin)
        x2 = Length.meters (-0.5 + xMargin)
        y = Length.meters 0
        z1 = Length.meters bottomMargin
        z2 = Length.meters (1 - topMargin)
    in
        Scene3d.quad material
           (Point3d.xyz x1 y z1)
           (Point3d.xyz x2 y z1)
           (Point3d.xyz x2 y z2)
           (Point3d.xyz x1 y z2)


subscription : Model -> Sub Msg
subscription model =
    case model of
        LoadingAssets { textures } ->
            Sub.map TexturesMsg Textures.subscription

        _ -> Sub.none


wallBlock : Model -> SceneEntity
wallBlock model =
    case model of
        ReadyAssets data -> data.wallBlock
        _ -> Scene3d.nothing

blueWallBlock : Model -> SceneEntity
blueWallBlock model =
    case model of
        ReadyAssets data -> data.blueWallBlock
        _ -> Scene3d.nothing

floorTile : Model -> SceneEntity
floorTile model =
    case model of
        ReadyAssets data -> data.floorTile
        _ -> Scene3d.nothing

ceilingTile : Model -> SceneEntity
ceilingTile model =
    case model of
        ReadyAssets data -> data.ceilingTile
        _ -> Scene3d.nothing

sign : Model -> String -> SceneEntity
sign model signId =
    case model of
        ReadyAssets data ->
            Dict.get signId data.signs
                |> Maybe.withDefault Scene3d.nothing
        _ -> Scene3d.nothing

sandTile : Model -> SceneEntity
sandTile model =
     case model of
        ReadyAssets data -> data.sandTile
        _ -> Scene3d.nothing

toyBucket : Model -> SceneEntity
toyBucket model =
    case model of
        ReadyAssets data -> data.toyBucket
        _ -> Scene3d.nothing

castleWall : Model -> SceneEntity
castleWall model =
    case model of
        ReadyAssets data -> data.castleWall
        _ -> Scene3d.nothing

castleDoor : Model -> SceneEntity
castleDoor model =
    case model of
        ReadyAssets data -> data.castleDoor
        _ -> Scene3d.nothing

castleWallTower : Model -> SceneEntity
castleWallTower model =
    case model of
        ReadyAssets data -> data.castleWallTower
        _ -> Scene3d.nothing

castleEntryVoid : Model -> SceneEntity
castleEntryVoid model =
    case model of
        ReadyAssets data -> data.castleEntryVoid
        _ -> Scene3d.nothing

chair : Model -> SceneEntity
chair model =
    case model of
        ReadyAssets data -> data.chair
        _ -> Scene3d.nothing


sandbox : Model -> SceneEntity
sandbox model =
    case model of
        ReadyAssets data -> data.sandbox
        _ -> Scene3d.nothing
