module MeshCollection exposing (Model(..), Msg, getMeshEntity, init, update)

import Color exposing (Color)
import Dict exposing (Dict)
import Http
import Length exposing (Meters)
import Obj.Decode exposing (Decoder, ObjCoordinates)
import Scene3d
import Scene3d.Material exposing (Texture)
import Scene3d.Mesh exposing (Textured, Uniform)
import Task exposing (Task)


type ViewMesh
    = TexturedMesh (Textured ObjCoordinates)
    | UniformMesh (Uniform ObjCoordinates)


meshDecoder : Decoder ViewMesh
meshDecoder =
    Obj.Decode.oneOf
        [ Obj.Decode.texturedFaces |> Obj.Decode.map (Scene3d.Mesh.texturedFaces >> TexturedMesh)
        , Obj.Decode.faces |> Obj.Decode.map (Scene3d.Mesh.indexedFaces >> UniformMesh)
        , Obj.Decode.texturedTriangles |> Obj.Decode.map (Scene3d.Mesh.texturedFacets >> TexturedMesh)
        , Obj.Decode.triangles |> Obj.Decode.map (Scene3d.Mesh.indexedFacets >> UniformMesh)
        ]


type Model
    = MeshCollectionInitializing
    | MeshCollectionLoaded (Dict String ViewMesh)
    | MeshCollectionFailed


type Msg
    = LoadedMeshes (Result String (List ( String, ViewMesh )))


init : List String -> ( Model, Cmd Msg )
init fileNames =
    ( MeshCollectionInitializing
    , fileNames
        |> List.map (\fileName -> Task.map (Tuple.pair fileName) (getMesh fileName))
        |> Task.sequence
        |> Task.attempt LoadedMeshes
    )


getMesh : String -> Task String ViewMesh
getMesh fileName =
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
                                meshDecoder
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


viewMesh : Maybe (Texture Color) -> ViewMesh -> Scene3d.Entity ObjCoordinates
viewMesh loadingTexture mesh =
    case mesh of
        TexturedMesh texturedMesh ->
            case loadingTexture of
                Just texture ->
                    Scene3d.mesh (Scene3d.Material.texturedMatte texture) texturedMesh

                Nothing ->
                    Scene3d.mesh (Scene3d.Material.matte Color.red) texturedMesh

        UniformMesh uniformMesh ->
            Scene3d.mesh (Scene3d.Material.matte Color.blue) uniformMesh
