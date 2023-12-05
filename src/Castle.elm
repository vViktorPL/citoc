module Castle exposing (dependencies, view)

import Angle
import Assets
import Axis3d
import Color
import Coordinates exposing (ObjectCoordinates)
import Frame3d
import Obj.Decode exposing (ObjCoordinates)
import Point3d
import Scene3d
import Scene3d.Material
import Vector3d


dependencies : List Assets.Dependency
dependencies =
    [ Assets.MeshDep "wall.obj"
    , Assets.MeshDep "wallDoor.obj"
    , Assets.MeshDep "wallCornerHalfTower.obj"
    , Assets.ColorTextureDep "Ground054_1K-JPG_Color.jpg"
    ]


view : Assets.Model -> Bool -> Scene3d.Entity ObjectCoordinates
view assets withVoidEntry =
    let
        sandMaterial =
            Scene3d.Material.texturedMatte (Assets.getColorTexture assets "Ground054_1K-JPG_Color.jpg")

        wallEntity =
            Assets.getMesh assets "wall.obj"
                |> Assets.texturedMesh
                |> Scene3d.mesh sandMaterial

        doorEntity =
            Assets.getMesh assets "wallDoor.obj"
                |> Assets.texturedMesh
                |> Scene3d.mesh sandMaterial

        wallTowerEntity =
            Assets.getMesh assets "wallCornerHalfTower.obj"
                |> Assets.texturedMesh
                |> Scene3d.mesh sandMaterial
    in
    Scene3d.group
        [ wallEntity
            |> Scene3d.placeIn (Frame3d.atPoint (Point3d.meters 0 -1 0))
        , doorEntity
            |> Scene3d.placeIn (Frame3d.atPoint (Point3d.meters 0 0 0))
        , wallEntity
            |> Scene3d.placeIn (Frame3d.atPoint (Point3d.meters 0 1 0))
        , wallTowerEntity
            |> Scene3d.rotateAround Axis3d.z (Angle.degrees -90)
            |> Scene3d.placeIn (Frame3d.atPoint (Point3d.meters -1 1 0))
        , wallTowerEntity
            |> Scene3d.rotateAround Axis3d.z (Angle.degrees -180)
            |> Scene3d.placeIn (Frame3d.atPoint (Point3d.meters -1 -2 0))
        , if withVoidEntry then
            Scene3d.quad
                (Scene3d.Material.color Color.black)
                (Point3d.unsafe { x = 0, y = 0, z = 0 })
                (Point3d.unsafe { x = 0, y = -1, z = 0 })
                (Point3d.unsafe { x = 0, y = -1, z = 1 })
                (Point3d.unsafe { x = 0, y = 0, z = 1 })
                |> Scene3d.placeIn (Frame3d.atPoint (Point3d.meters -0.25 0 0))

          else
            Scene3d.nothing
        ]
        |> Scene3d.translateBy (Vector3d.unsafe { x = 0, y = 0.5, z = 0 })
