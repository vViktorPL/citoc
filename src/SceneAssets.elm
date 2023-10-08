module SceneAssets exposing (Model, Msg, init, update, subscription, ready, wallBlock, blueWallBlock, floorTile, ceilingTile, sign, sandTile, toyBucket)

import Textures exposing (TextureToLoad(..), TexturesState(..))
import MeshCollection exposing (Model(..))
import Scene3d
import Scene3d.Material
import Length exposing (Length, Meters)
import Obj.Decode exposing (ObjCoordinates)
import Dict exposing (Dict)
import Point3d
import Luminance

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
    , TextureColor "SofaChair_Base_Color.png"
    , TextureColor "Ground054_1K-JPG_Color.jpg"
    , TextureFloat "Ground054_1K-JPG_Roughness.jpg"
    , TextureColor "ToyBucket.png"
    ]

meshesToLoad =
    [ "ToyBucket.obj"
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
                                 |> Maybe.map (Scene3d.scaleAbout (Point3d.meters 0 0 0) 0.4)
                                 |> Maybe.withDefault Scene3d.nothing
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


--
--  x1 = Length.meters (toFloat -x)
  --       y1 = Length.meters (toFloat (y + 1))

  --       x2 = Length.meters (toFloat -x - 1)
  --       y2 = Length.meters (toFloat (y + 1))
  --       x3 = Length.meters (toFloat -x - 1)
  --       y3 = Length.meters (toFloat y)
  --       x4 = Length.meters (toFloat -x)
  --       y4 = Length.meters (toFloat y)

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
