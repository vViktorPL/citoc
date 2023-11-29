port module Textures exposing
    ( LoadedTexture
    , Model
    , Msg
    , TextureToLoad(..)
    , TexturesState(..)
    , getState
    , getTexture
    , getTextureFloat
    , init
    , load
    , loadedSignTextureFileName
    , subscription
    , update
    )

import Color exposing (Color)
import Dict exposing (Dict)
import Scene3d.Material as Material
import Task


port generateTexturePort : ( String, String ) -> Cmd msg


port generatedSignTextureSub : (( String, String ) -> msg) -> Sub msg


type Model
    = Textures TexturesState (Dict String LoadedTexture)


type TexturesState
    = RemainingTextures Int
    | TextureInitError
    | TexturesLoaded


getState : Model -> TexturesState
getState (Textures state _) =
    state


type LoadedTexture
    = LoadedTextureColor (Material.Texture Color)
    | LoadedTextureFloat (Material.Texture Float)


type TextureToLoad
    = TextureColor String
    | TextureFloat String
    | GenerateSign String String


type Msg
    = StandardTexturesLoaded (Dict String LoadedTexture)
    | SignTextureGenerated String String
    | SignTextureLoaded String LoadedTexture
    | TextureLoadingError


loadedSignTextureFileName : Msg -> Maybe String
loadedSignTextureFileName msg =
    case msg of
        SignTextureLoaded fileName _ ->
            Just fileName

        _ ->
            Nothing


getTexture : Model -> String -> Maybe (Material.Texture Color)
getTexture (Textures _ data) fileName =
    case Dict.get fileName data of
        Just (LoadedTextureColor texture) ->
            Just texture

        _ ->
            Nothing


getTextureFloat : Model -> String -> Maybe (Material.Texture Float)
getTextureFloat (Textures _ data) fileName =
    case Dict.get fileName data of
        Just (LoadedTextureFloat texture) ->
            Just texture

        _ ->
            Nothing


loadTexture fileName =
    loadTextureFromUrl ("assets/" ++ fileName)


loadTextureFromUrl =
    Material.loadWith Material.trilinearFiltering


init : List TextureToLoad -> ( Model, Cmd Msg )
init texturesToLoad =
    let
        standardTexturesLoadingTasks =
            texturesToLoad
                |> List.filterMap
                    (\textureType ->
                        case textureType of
                            TextureColor fileName ->
                                loadTexture fileName
                                    |> Task.map LoadedTextureColor
                                    |> Task.map (Tuple.pair fileName)
                                    |> Just

                            TextureFloat fileName ->
                                loadTexture fileName
                                    |> Task.map LoadedTextureFloat
                                    |> Task.map (Tuple.pair fileName)
                                    |> Just

                            _ ->
                                Nothing
                    )

        generateTexturesCommands =
            texturesToLoad
                |> List.filterMap
                    (\textureType ->
                        case textureType of
                            GenerateSign fileName text ->
                                Just (generateTexturePort ( fileName, text ))

                            _ ->
                                Nothing
                    )

        texturesToLoadCount =
            List.length standardTexturesLoadingTasks + List.length generateTexturesCommands
    in
    ( Textures (RemainingTextures texturesToLoadCount) Dict.empty
    , Cmd.batch
        [ standardTexturesLoadingTasks
            |> Task.sequence
            |> Task.map Dict.fromList
            |> Task.attempt
                (\result ->
                    case result of
                        Ok loadedTextures ->
                            StandardTexturesLoaded loadedTextures

                        Err _ ->
                            TextureLoadingError
                )
        , Cmd.batch generateTexturesCommands
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StandardTexturesLoaded textures ->
            updateStandardTexturesLoaded textures model

        SignTextureGenerated fileName url ->
            updateSignTextureGenerated fileName url model

        SignTextureLoaded fileName texture ->
            updateSignTextureLoaded fileName texture model

        TextureLoadingError ->
            ( Textures TextureInitError Dict.empty, Cmd.none )


updateStandardTexturesLoaded : Dict String LoadedTexture -> Model -> ( Model, Cmd Msg )
updateStandardTexturesLoaded newTextures (Textures state prevTextures) =
    let
        newRemaining =
            case state of
                RemainingTextures remaining ->
                    remaining - Dict.size newTextures

                _ ->
                    0

        combinedTextures =
            Dict.union prevTextures newTextures
    in
    if newRemaining <= 0 then
        ( Textures TexturesLoaded combinedTextures, Cmd.none )

    else
        ( Textures (RemainingTextures newRemaining) combinedTextures, Cmd.none )


updateSignTextureGenerated : String -> String -> Model -> ( Model, Cmd Msg )
updateSignTextureGenerated fileName url (Textures state textures) =
    let
        newRemaining =
            case state of
                RemainingTextures remaining ->
                    remaining - 1

                _ ->
                    0

        newState =
            if newRemaining <= 0 then
                TexturesLoaded

            else
                RemainingTextures newRemaining
    in
    ( Textures newState textures
    , loadTextureFromUrl url
        |> Task.attempt
            (\result ->
                case result of
                    Ok loadedTexture ->
                        SignTextureLoaded fileName (LoadedTextureColor loadedTexture)

                    Err _ ->
                        TextureLoadingError
            )
    )


updateSignTextureLoaded : String -> LoadedTexture -> Model -> ( Model, Cmd Msg )
updateSignTextureLoaded fileName loadedTexture (Textures state textures) =
    let
        newTextures =
            Dict.insert fileName loadedTexture textures
    in
    ( Textures state newTextures, Cmd.none )


subscription : Sub Msg
subscription =
    generatedSignTextureSub (\( fileName, url ) -> SignTextureGenerated fileName url)


load : Model -> TextureToLoad -> ( Model, Cmd Msg )
load (Textures state textures) textureToLoad =
    let
        loadCmd toMsg textureTask =
            textureTask
                |> Task.attempt
                    (\result ->
                        case result of
                            Ok loadedTexture ->
                                toMsg loadedTexture

                            Err _ ->
                                TextureLoadingError
                    )
    in
    case textureToLoad of
        TextureColor fileName ->
            ( Textures state textures
            , loadTexture fileName
                |> Task.map LoadedTextureColor
                |> loadCmd (SignTextureLoaded fileName)
            )

        TextureFloat fileName ->
            ( Textures state textures
            , loadTexture fileName
                |> Task.map LoadedTextureFloat
                |> loadCmd (SignTextureLoaded fileName)
            )

        GenerateSign fileName text ->
            ( Textures state textures
            , generateTexturePort ( fileName, text )
            )
