port module Assets exposing
    ( Dependency(..)
    , Mesh
    , Model
    , Msg
    , areLoading
    , areReady
    , getColorTexture
    , getMesh
    , getMeshCollection
    , getOtherTexture
    , getSignText
    , getSignTexture
    , init
    , loadingProgressPercentage
    , meshVertices
    , requestDependencies
    , subscription
    , texturedMesh
    , update
    )

import Color exposing (Color)
import Dict exposing (Dict)
import Http
import Length
import Obj.Decode exposing (Decoder, ObjCoordinates)
import Point3d exposing (Point3d)
import Scene3d.Material
import Scene3d.Mesh exposing (Textured, Uniform)
import Task exposing (Task)
import TriangularMesh exposing (TriangularMesh)
import WebGL.Texture


port generateTexturePort : ( String, String ) -> Cmd msg


port generatedSignTextureSub : (( String, String ) -> msg) -> Sub msg


port preloadSound : String -> Cmd msg


port preloadMusic : String -> Cmd msg


port soundPreloadedSub : (String -> msg) -> Sub msg


port musicPreloadedSub : (String -> msg) -> Sub msg


type Model
    = Assets RequestQueue AssetsData


type Dependency
    = ColorTextureDep String
    | OtherTextureDep String
    | SignTextureDep String String
    | MeshDep String
    | MeshCollectionDep String
    | SoundEffectDep String
    | MusicDep String


type Msg
    = ColorTextureLoaded String ColorTexture
    | OtherTextureLoaded String OtherTexture
    | SignTextureLoaded String ColorTexture
    | SignTextureLoadError WebGL.Texture.Error
    | SignTextureGenerated { name : String, dataUri : String }
    | TextureLoadError WebGL.Texture.Error
    | MeshLoaded String Mesh
    | MeshLoadError String
    | MeshCollectionLoaded String (List Mesh)
    | MeshCollectionLoadError String
    | SoundLoaded String
    | MusicLoaded String


type Mesh
    = Mesh MeshVertices TexturedMesh


type alias MeshVertices =
    TriangularMesh (Point3d Length.Meters ObjCoordinates)


type alias TexturedMesh =
    Textured ObjCoordinates


type alias ColorTexture =
    Scene3d.Material.Texture Color


type alias OtherTexture =
    Scene3d.Material.Texture Float


type alias RequestQueue =
    { pending : List Dependency
    , loaded : List Dependency
    }


type alias AssetsData =
    { colorTextures : Dict String ColorTexture
    , otherTextures : Dict String OtherTexture
    , signTextures : Dict String ColorTexture
    , signTexts : Dict String String
    , meshes : Dict String Mesh
    , meshCollections : Dict String (List Mesh)
    }


getColorTexture : Model -> String -> ColorTexture
getColorTexture (Assets _ data) fileName =
    Dict.get fileName data.colorTextures
        |> Maybe.withDefault (Scene3d.Material.constant Color.black)


getOtherTexture : Model -> String -> OtherTexture
getOtherTexture (Assets _ data) fileName =
    Dict.get fileName data.otherTextures
        |> Maybe.withDefault (Scene3d.Material.constant 1)


getSignTexture : Model -> String -> ColorTexture
getSignTexture (Assets _ data) name =
    Dict.get name data.signTextures
        |> Maybe.withDefault (Scene3d.Material.constant Color.lightBrown)


getSignText : Model -> String -> String
getSignText (Assets _ data) name =
    Dict.get name data.signTexts
        |> Maybe.withDefault ""


emptyMesh : Mesh
emptyMesh =
    Mesh TriangularMesh.empty (Scene3d.Mesh.texturedFaces TriangularMesh.empty)


getMesh : Model -> String -> Mesh
getMesh (Assets _ data) fileName =
    Dict.get fileName data.meshes
        |> Maybe.withDefault emptyMesh


meshVertices : Mesh -> MeshVertices
meshVertices (Mesh vertices _) =
    vertices


texturedMesh : Mesh -> TexturedMesh
texturedMesh (Mesh _ tMesh) =
    tMesh


getMeshCollection : Model -> String -> List Mesh
getMeshCollection (Assets _ data) fileName =
    Dict.get fileName data.meshCollections
        |> Maybe.withDefault []


areLoading : Model -> Bool
areLoading =
    areReady >> not


areReady : Model -> Bool
areReady (Assets { pending } _) =
    List.isEmpty pending


init : Model
init =
    Assets
        { pending = [], loaded = [] }
        { colorTextures = Dict.empty
        , otherTextures = Dict.empty
        , signTextures = Dict.empty
        , signTexts = Dict.empty
        , meshes = Dict.empty
        , meshCollections = Dict.empty
        }


requestDependencies : List Dependency -> Model -> ( Model, Cmd Msg )
requestDependencies dependencies (Assets requestQueue data) =
    let
        missingDependencies =
            List.filter
                (\dependency -> not (List.member dependency requestQueue.pending || List.member dependency requestQueue.loaded))
                dependencies

        newSignTexts =
            dependencies
                |> List.filterMap
                    (\dependency ->
                        case dependency of
                            SignTextureDep name text ->
                                Just ( name, text )

                            _ ->
                                Nothing
                    )
                |> Dict.fromList
    in
    ( Assets
        { requestQueue | pending = requestQueue.pending ++ missingDependencies }
        { data | signTexts = Dict.union newSignTexts data.signTexts }
    , missingDependencies
        |> List.map loadDependency
        |> Cmd.batch
    )


markAssetAsLoaded : Dependency -> RequestQueue -> RequestQueue
markAssetAsLoaded loadedDependency queue =
    { pending = List.filter ((/=) loadedDependency) queue.pending
    , loaded = loadedDependency :: queue.loaded
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Assets queue data) =
    case msg of
        ColorTextureLoaded fileName texture ->
            ( Assets
                (markAssetAsLoaded (ColorTextureDep fileName) queue)
                { data | colorTextures = Dict.insert fileName texture data.colorTextures }
            , Cmd.none
            )

        OtherTextureLoaded fileName texture ->
            ( Assets
                (markAssetAsLoaded (OtherTextureDep fileName) queue)
                { data | otherTextures = Dict.insert fileName texture data.otherTextures }
            , Cmd.none
            )

        SignTextureLoaded fileName texture ->
            let
                text =
                    Dict.get fileName data.signTexts
                        |> Maybe.withDefault ""
            in
            ( Assets
                (markAssetAsLoaded (SignTextureDep fileName text) queue)
                { data | signTextures = Dict.insert fileName texture data.signTextures }
            , Cmd.none
            )

        SignTextureGenerated { name, dataUri } ->
            ( Assets queue data
            , dataUri
                |> Scene3d.Material.load
                |> Task.attempt
                    (\result ->
                        case result of
                            Ok texture ->
                                SignTextureLoaded name texture

                            Err err ->
                                SignTextureLoadError err
                    )
            )

        MeshLoaded fileName mesh ->
            ( Assets
                (markAssetAsLoaded (MeshDep fileName) queue)
                { data | meshes = Dict.insert fileName mesh data.meshes }
            , Cmd.none
            )

        MeshCollectionLoaded fileName meshes ->
            ( Assets
                (markAssetAsLoaded (MeshCollectionDep fileName) queue)
                { data | meshCollections = Dict.insert fileName meshes data.meshCollections }
            , Cmd.none
            )

        SoundLoaded fileName ->
            ( Assets
                (markAssetAsLoaded (SoundEffectDep fileName) queue)
                data
            , Cmd.none
            )

        MusicLoaded fileName ->
            ( Assets
                (markAssetAsLoaded (MusicDep fileName) queue)
                data
            , Cmd.none
            )

        -- TODO: error handling
        _ ->
            ( Assets queue data, Cmd.none )


loadDependency : Dependency -> Cmd Msg
loadDependency dependency =
    case dependency of
        ColorTextureDep fileName ->
            ("assets/" ++ fileName)
                |> Scene3d.Material.load
                |> Task.attempt
                    (\result ->
                        case result of
                            Ok texture ->
                                ColorTextureLoaded fileName texture

                            Err err ->
                                TextureLoadError err
                    )

        OtherTextureDep fileName ->
            ("assets/" ++ fileName)
                |> Scene3d.Material.load
                |> Task.attempt
                    (\result ->
                        case result of
                            Ok texture ->
                                OtherTextureLoaded fileName texture

                            Err err ->
                                TextureLoadError err
                    )

        SignTextureDep name text ->
            generateTexturePort ( name, text )

        MeshDep fileName ->
            getMeshTask fileName meshDecoder
                |> Task.attempt
                    (\result ->
                        case result of
                            Ok mesh ->
                                MeshLoaded fileName mesh

                            Err err ->
                                MeshLoadError err
                    )

        MeshCollectionDep fileName ->
            getMeshTask fileName meshCollectionDecoder
                |> Task.attempt
                    (\result ->
                        case result of
                            Ok meshes ->
                                MeshCollectionLoaded fileName meshes

                            Err err ->
                                MeshCollectionLoadError err
                    )

        SoundEffectDep fileName ->
            preloadSound fileName

        MusicDep fileName ->
            preloadMusic fileName


loadingProgressPercentage : Model -> Int
loadingProgressPercentage (Assets requestQueue _) =
    let
        pendingAssetsCount =
            List.length requestQueue.pending

        loadedAssetsCount =
            List.length requestQueue.loaded
    in
    if pendingAssetsCount == 0 then
        100

    else
        (toFloat loadedAssetsCount / toFloat (loadedAssetsCount + pendingAssetsCount))
            * 100
            |> floor


getMeshTask : String -> Obj.Decode.Decoder a -> Task String a
getMeshTask fileName decoder =
    Http.task
        { method = "GET"
        , headers = []
        , url = "assets/" ++ fileName
        , body = Http.emptyBody
        , resolver =
            Http.stringResolver
                (\response ->
                    case response of
                        Http.BadUrl_ url ->
                            Err "Bad URL"

                        Http.Timeout_ ->
                            Err "Timeout"

                        Http.BadStatus_ { statusCode } _ ->
                            Err "Bad status"

                        Http.NetworkError_ ->
                            Err "Network error"

                        Http.GoodStatus_ _ body ->
                            Obj.Decode.decodeString
                                Length.meters
                                decoder
                                body
                )
        , timeout = Nothing
        }


meshDecoder : Decoder Mesh
meshDecoder =
    Obj.Decode.oneOf
        [ Obj.Decode.texturedFaces
            |> Obj.Decode.map (\decodedMesh -> Mesh (TriangularMesh.mapVertices .position decodedMesh) (Scene3d.Mesh.texturedFaces decodedMesh))
        , Obj.Decode.texturedTriangles
            |> Obj.Decode.map (\decodedMesh -> Mesh (TriangularMesh.mapVertices .position decodedMesh) (Scene3d.Mesh.texturedFacets decodedMesh))
        ]


meshCollectionDecoder : Decoder (List Mesh)
meshCollectionDecoder =
    Obj.Decode.objectNames
        |> Obj.Decode.andThen (List.map (\objName -> Obj.Decode.object objName meshDecoder) >> Obj.Decode.combine)


subscription : Sub Msg
subscription =
    Sub.batch
        [ generatedSignTextureSub (\( name, dataUri ) -> SignTextureGenerated { name = name, dataUri = dataUri })
        , soundPreloadedSub SoundLoaded
        , musicPreloadedSub MusicLoaded
        ]
