module BreakableWall exposing (Model, init, view, break, update)

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
import TriangularMesh
import Scene3d.Mesh
import Scene3d.Material exposing (Material)
import Color exposing (Color)
import Frame3d

type Model
    = BreakableWall (List (Scene3d.Entity WorldCoordinates))
    --| BreakingWall


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

init : Material WorldCoordinates { uvs : () } -> Model
init material =
    let
        texturedTriangles = DelaunayTriangulation2d.fromPoints random2DPoints
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
                        (TriangularMesh.triangles
                            [({ position = Point3d.meters -p1m.x 0.1 (1 - p1m.y), uv = (p1m.x, p1m.y) }
                            , { position = Point3d.meters -p2m.x 0 (1 - p2m.y), uv = (p2m.x, p2m.y) }
                            , { position = Point3d.meters -p3m.x 0 (1 - p3m.y), uv = (p3m.x, p3m.y) }
                            )]
                        )
                            |> Scene3d.Mesh.texturedTriangles

                )

        segments = List.map (Scene3d.mesh material) texturedTriangles
    in
    BreakableWall segments


view : Model -> Scene3d.Entity WorldCoordinates
view model =
    case model of
        BreakableWall segments -> Scene3d.group segments

break : Point3d Length.Meters WorldCoordinates -> Vector3d Length.Meters WorldCoordinates -> Model -> Model
break collisionPoint forceVector model = model

update : Float -> Model -> Model
update delta model = model
