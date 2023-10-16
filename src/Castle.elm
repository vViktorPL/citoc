module Castle exposing (view)

import Scene3d
import Frame3d
import Axis3d
import Point3d
import Angle

import Obj.Decode exposing (ObjCoordinates)
import SceneAssets

type alias WorldCoordinates = ObjCoordinates

view : SceneAssets.Model -> (Int, Int) -> Scene3d.Entity WorldCoordinates
view sceneAssets (x, y) =
    Scene3d.group
        [ SceneAssets.castleWall sceneAssets
            |> Scene3d.placeIn (Frame3d.atPoint (Point3d.meters 0 -1 0))
        , SceneAssets.castleDoor sceneAssets
            |> Scene3d.placeIn (Frame3d.atPoint (Point3d.meters 0 0 0))
         , SceneAssets.castleWall sceneAssets
            |> Scene3d.placeIn (Frame3d.atPoint (Point3d.meters 0 1 0))
        , SceneAssets.castleWallTower sceneAssets
            |> Scene3d.rotateAround Axis3d.z (Angle.degrees -90)
            |> Scene3d.placeIn (Frame3d.atPoint (Point3d.meters -1 1 0))
        , SceneAssets.castleWallTower sceneAssets
            |> Scene3d.rotateAround Axis3d.z (Angle.degrees -180)
            |> Scene3d.placeIn (Frame3d.atPoint (Point3d.meters -1 -2 0))
        , SceneAssets.castleEntryVoid sceneAssets
            |> Scene3d.placeIn (Frame3d.atPoint (Point3d.meters -0.5 0 0))
        ]
        |> Scene3d.scaleAbout Point3d.origin 3
        |> Scene3d.rotateAround Axis3d.z (Angle.degrees -90)
        |> Scene3d.placeIn (Frame3d.atPoint (Point3d.meters (-(toFloat x) + 1)  (toFloat y) 0))
