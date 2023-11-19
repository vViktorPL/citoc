module MeshCollection exposing (MeshLoadMode(..), Model(..), Msg, getMeshEntity, getMeshSubentities, getMeshVertices, init, update)

import Color exposing (Color)
import Dict exposing (Dict)
import Http
import Length exposing (Meters)
import Obj.Decode exposing (Decoder, ObjCoordinates)
import Point3d exposing (Point3d)
import Scene3d
import Scene3d.Material exposing (Texture)
import Scene3d.Mesh exposing (Textured, Uniform)
import Task exposing (Task)
import TriangularMesh exposing (TriangularMesh)


type ViewMesh
    = TexturedMesh (TriangularMesh (Point3d Length.Meters ObjCoordinates)) (Textured ObjCoordinates)
    | UniformMesh (TriangularMesh (Point3d Length.Meters ObjCoordinates)) (Uniform ObjCoordinates)
    | MeshGroup (List ViewMesh)


meshDecoder : Decoder ViewMesh
meshDecoder =
    Obj.Decode.oneOf
        [ Obj.Decode.texturedFaces
            |> Obj.Decode.map (\decodedMesh -> TexturedMesh (TriangularMesh.mapVertices .position decodedMesh) (Scene3d.Mesh.texturedFaces decodedMesh))
        , Obj.Decode.faces
            |> Obj.Decode.map (\decodedMesh -> UniformMesh (TriangularMesh.mapVertices .position decodedMesh) (Scene3d.Mesh.indexedFaces decodedMesh))
        , Obj.Decode.texturedTriangles
            |> Obj.Decode.map (\decodedMesh -> TexturedMesh (TriangularMesh.mapVertices .position decodedMesh) (Scene3d.Mesh.texturedFacets decodedMesh))
        , Obj.Decode.triangles
            |> Obj.Decode.map (\decodedMesh -> UniformMesh decodedMesh (Scene3d.Mesh.indexedFacets decodedMesh))
        ]


groupedMeshDecoder : Decoder ViewMesh
groupedMeshDecoder =
    Obj.Decode.objectNames
        |> Obj.Decode.andThen (List.map (\objName -> Obj.Decode.object objName meshDecoder) >> Obj.Decode.combine >> Obj.Decode.map MeshGroup)


type Model
    = MeshCollectionInitializing
    | MeshCollectionLoaded (Dict String ViewMesh)
    | MeshCollectionFailed


type Msg
    = LoadedMeshes (Result String (List ( String, ViewMesh )))


type MeshLoadMode
    = SingleEntity
    | Subentities


init : List ( String, MeshLoadMode ) -> ( Model, Cmd Msg )
init fileNames =
    ( MeshCollectionInitializing
    , fileNames
        |> List.map
            (\( fileName, loadMode ) ->
                Task.map (Tuple.pair fileName)
                    (case loadMode of
                        SingleEntity ->
                            getMesh fileName

                        Subentities ->
                            getGroupedMesh fileName
                    )
            )
        |> Task.sequence
        |> Task.attempt LoadedMeshes
    )


getMesh : String -> Task String ViewMesh
getMesh fileName =
    getMeshHelp fileName meshDecoder


getGroupedMesh : String -> Task String ViewMesh
getGroupedMesh fileName =
    getMeshHelp fileName groupedMeshDecoder


getMeshHelp : String -> Obj.Decode.Decoder a -> Task String a
getMeshHelp fileName decoder =
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadedMeshes result ->
            ( result
                |> Result.map (Dict.fromList >> MeshCollectionLoaded)
                |> Result.withDefault MeshCollectionFailed
            , Cmd.none
            )


getMeshEntity : Model -> String -> Maybe (Texture Color) -> Maybe (Scene3d.Entity ObjCoordinates)
getMeshEntity model meshFileName texture =
    case model of
        MeshCollectionLoaded dict ->
            Dict.get meshFileName dict
                |> Maybe.map (viewMesh texture)

        _ ->
            Nothing


getMeshSubentities : Model -> String -> Maybe (Texture Color) -> List (Scene3d.Entity ObjCoordinates)
getMeshSubentities model meshFileName texture =
    case model of
        MeshCollectionLoaded dict ->
            Dict.get meshFileName dict
                |> Maybe.map meshToSubmeshesList
                |> Maybe.map (List.map (viewMesh texture))
                |> Maybe.withDefault []

        _ ->
            []


getMeshVertices : Model -> String -> List (TriangularMesh (Point3d Length.Meters ObjCoordinates))
getMeshVertices model meshFileName =
    case model of
        MeshCollectionLoaded dict ->
            Dict.get meshFileName dict
                |> Maybe.map getMeshVerticesHelp
                |> Maybe.withDefault []

        _ ->
            []


getMeshVerticesHelp : ViewMesh -> List (TriangularMesh (Point3d Length.Meters ObjCoordinates))
getMeshVerticesHelp mesh =
    case mesh of
        TexturedMesh vertices _ ->
            [ vertices ]

        UniformMesh vertices _ ->
            [ vertices ]

        MeshGroup meshes ->
            meshes
                |> List.concatMap getMeshVerticesHelp


meshToSubmeshesList : ViewMesh -> List ViewMesh
meshToSubmeshesList mesh =
    case mesh of
        MeshGroup submeshes ->
            List.concatMap meshToSubmeshesList submeshes

        singleMesh ->
            List.singleton singleMesh


viewMesh : Maybe (Texture Color) -> ViewMesh -> Scene3d.Entity ObjCoordinates
viewMesh loadingTexture mesh =
    case mesh of
        TexturedMesh _ texturedMesh ->
            case loadingTexture of
                Just texture ->
                    Scene3d.mesh (Scene3d.Material.texturedMatte texture) texturedMesh

                Nothing ->
                    Scene3d.mesh (Scene3d.Material.matte Color.red) texturedMesh

        UniformMesh _ uniformMesh ->
            Scene3d.mesh (Scene3d.Material.matte Color.blue) uniformMesh

        MeshGroup meshes ->
            meshes
                |> List.map (viewMesh loadingTexture)
                |> Scene3d.group
