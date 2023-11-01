module BreakableWall exposing (Model, init, view, update)

import Obj.Decode exposing (ObjCoordinates)

import Random

import DelaunayTriangulation2d

import Vector3d exposing (Vector3d)
import Point2d
import Point3d exposing (Point3d)
import Length
import Array
import Triangle2d
import Scene3d
import TriangularMesh exposing (TriangularMesh)
import Scene3d.Mesh
import Scene3d.Material exposing (Material)
import Color exposing (Color)
import Frame3d
import Physics.Body
import Physics.Coordinates exposing (BodyCoordinates)
import Physics.Shape
import Physics.World exposing (World)
import Length exposing (Meters)
import Acceleration
import Direction3d
import Duration
import Mass

type Model
    = BreakableWall
        { physics: (World (Scene3d.Entity BodyCoordinates))
        , active: Bool
        }
     --(List (TriangularMesh (Point3d Meters BodyCoordinates)))
    --| BreakingWall (World (Scene3d.Entity WorldCoordinates))


type alias WorldCoordinates = ObjCoordinates

random2DPointGenerator =
    Random.pair (Random.float 0 1) (Random.float 0 1)

random2DPoints =
    Random.step
        (Random.list 50 random2DPointGenerator)
        (Random.initialSeed 777)
        |> Tuple.first
        |> List.append [(0, 0), (1, 0), (0, 1), (1, 1)]
        |> List.map (\(x, y) -> Point2d.meters x y)
        |> Array.fromList

--init : Material WorldCoordinates { uvs : () } -> Model
init material =
    let
        vertexesWithUvs = DelaunayTriangulation2d.fromPoints random2DPoints
            |> Result.withDefault DelaunayTriangulation2d.empty
            |> DelaunayTriangulation2d.triangles
            |> List.map
                (\triangle ->
                    let
                        (p1, p2, p3) = Triangle2d.vertices triangle
                        p1m = Point2d.toMeters p1
                        p2m = Point2d.toMeters p2
                        p3m = Point2d.toMeters p3
                    in
                        ( { position = Point3d.meters -p1m.x 0.1 (1 - p1m.y), uv = (p1m.x, p1m.y) }
                        , { position = Point3d.meters -p2m.x 0 (1 - p2m.y), uv = (p2m.x, p2m.y) }
                        , { position = Point3d.meters -p3m.x 0 (1 - p3m.y), uv = (p3m.x, p3m.y) }
                        )
                )

        triangles =
            vertexesWithUvs
                |> List.map
                    (\(v1, v2, v3) ->
                        ( TriangularMesh.triangles [(v1.position, v2.position, v3.position)]
                        , TriangularMesh.triangles [(v1, v2, v3)]
                        )
                    )

        physics = List.foldl
            (\(triangleForPhysics, triangleWithUv) world ->
              let
                  sceneEntity =
                      triangleWithUv
                          |> Scene3d.Mesh.texturedTriangles
                          |> Scene3d.mesh material
                  body =
                      Physics.Body.compound [Physics.Shape.unsafeConvex triangleForPhysics] sceneEntity
                        |> Physics.Body.withBehavior (Physics.Body.dynamic (Mass.kilograms 1))
              in
              world
                  |> Physics.World.add body
            )
            (Physics.World.withGravity (Acceleration.metersPerSecondSquared 9.80665) Direction3d.negativeZ Physics.World.empty)
            triangles
    in
    BreakableWall
        { physics = physics
        , active = False
        }


--view : Model -> Scene3d.Entity BodyCoordinates
view (BreakableWall { physics }) =
    physics
        |> Physics.World.bodies
        |> List.map
            (\body ->
                let
                    frame3d =
                        Physics.Body.frame body
                in
                    Physics.Body.data body
                        |> Scene3d.placeIn frame3d
            )
        |> Scene3d.group

--break : Point3d Length.Meters WorldCoordinates -> Vector3d Length.Meters WorldCoordinates -> Model -> Model
--break collisionPoint forceVector (BreakableWall model) =
--    BreakableWall
--        { physics =
--
--        }

update : Float -> Model -> Model
update delta (BreakableWall model) =
    BreakableWall {
        model | physics = Physics.World.simulate (Duration.milliseconds delta) model.physics
    }

