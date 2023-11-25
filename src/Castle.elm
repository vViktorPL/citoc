module Castle exposing (view)

import Angle
import Axis3d
import Frame3d
import Obj.Decode exposing (ObjCoordinates)
import Point3d
import Scene3d
import SceneAssets
import Vector3d


type alias WorldCoordinates =
    ObjCoordinates


view : SceneAssets.Model -> Bool -> Scene3d.Entity WorldCoordinates
view sceneAssets withVoidEntry =
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
        , if withVoidEntry then
            SceneAssets.castleEntryVoid sceneAssets
                |> Scene3d.placeIn (Frame3d.atPoint (Point3d.meters -0.25 0 0))

          else
            Scene3d.nothing
        ]
        --|> Scene3d.scaleAbout Point3d.origin scale
        |> Scene3d.translateBy (Vector3d.unsafe { x = 0, y = 0.5, z = 0 })



--|> Scene3d.placeIn (Frame3d.atPoint (Point3d.meters (-(toFloat x)) (toFloat y) 0))
