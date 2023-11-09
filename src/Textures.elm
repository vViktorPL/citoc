port module Textures exposing (LoadedTexture, Model, Msg, TextureToLoad(..), TexturesState(..), getState, getTexture, getTextureFloat, init, subscription, update)

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
update msg (Textures state prevTextures) =
    case ( msg, state ) of
        ( StandardTexturesLoaded textures, RemainingTextures remaining ) ->
            let
                newRemaining =
                    remaining - Dict.size textures

                newTextures =
                    Dict.union prevTextures textures
            in
            if newRemaining <= 0 then
                ( Textures TexturesLoaded newTextures, Cmd.none )

            else
                ( Textures (RemainingTextures newRemaining) newTextures, Cmd.none )

        ( SignTextureGenerated fileName url, RemainingTextures _ ) ->
            ( Textures state prevTextures
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

        ( SignTextureLoaded fileName texture, RemainingTextures remaining ) ->
            let
                newRemaining =
                    remaining - 1

                newTextures =
                    Dict.insert fileName texture prevTextures
            in
            if newRemaining <= 0 then
                ( Textures TexturesLoaded newTextures, Cmd.none )

            else
                ( Textures (RemainingTextures newRemaining) newTextures, Cmd.none )

        ( TextureLoadingError, _ ) ->
            ( Textures TextureInitError Dict.empty, Cmd.none )

        _ ->
            ( Textures state prevTextures, Cmd.none )


subscription : Sub Msg
subscription =
    generatedSignTextureSub (\( fileName, url ) -> SignTextureGenerated fileName url)
