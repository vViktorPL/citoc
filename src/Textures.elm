module Textures exposing (Textures, getTexture, loadTextures)

import Dict exposing (Dict)
import Scene3d.Material as Material
import Color exposing (Color)
import Task

type alias Textures
    = Dict String (Material.Texture Color)

getTexture : Textures -> String -> Maybe (Material.Texture Color)
getTexture textures fileName =
    Dict.get fileName textures

--loadTexture : String -> Cmd Msg
loadTexture fileName =
   Material.loadWith Material.trilinearFiltering ("assets/" ++ fileName)

--loadTextures : List String -> Task
loadTextures fileNames =
    fileNames
        |> List.map (\fileName -> loadTexture fileName |> Task.map (Tuple.pair fileName))
        |> Task.sequence
        |> Task.map Dict.fromList
