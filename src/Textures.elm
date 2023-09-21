module Textures exposing (Textures, getTexture, getTextureFloat, loadTextures, LoadedTexture, TextureType(..))

import Dict exposing (Dict)
import Scene3d.Material as Material
import Color exposing (Color)
import Task

type alias Textures
    = Dict String LoadedTexture

type LoadedTexture
    = LoadedTextureColor (Material.Texture Color)
    | LoadedTextureFloat (Material.Texture Float)

type TextureType = TextureColor | TextureFloat

getTexture : Textures -> String -> Maybe (Material.Texture Color)
getTexture textures fileName =
    case Dict.get fileName textures of
        Just (LoadedTextureColor texture) -> Just texture
        _ -> Nothing

getTextureFloat : Textures -> String -> Maybe (Material.Texture Float)
getTextureFloat textures fileName =
    case Dict.get fileName textures of
        Just (LoadedTextureFloat texture) -> Just texture
        _ -> Nothing

loadTexture fileName =
    Material.loadWith Material.trilinearFiltering ("assets/" ++ fileName)


--loadTextures : List (TextureType, String) -> Textures
loadTextures texturesToLoad =
    texturesToLoad
        |> List.map (\(textureType, fileName) ->
            case textureType of
                TextureColor ->
                    loadTexture fileName
                        |> Task.map LoadedTextureColor
                        |> Task.map (Tuple.pair fileName)
                TextureFloat ->
                    loadTexture fileName
                        |> Task.map LoadedTextureFloat
                        |> Task.map (Tuple.pair fileName)
        )
        |> Task.sequence
        |> Task.map Dict.fromList
