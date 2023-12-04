module BreakableWall exposing (Model, break, init, isBroken, update, view)

import Acceleration
import Array
import Assets
import Coordinates exposing (ObjectCoordinates)
import DelaunayTriangulation2d
import Direction3d
import Duration
import Force
import Frame3d
import Length exposing (Meters)
import Mass
import Obj.Decode exposing (ObjCoordinates)
import Physics.Body
import Physics.Coordinates exposing (BodyCoordinates, WorldCoordinates)
import Physics.Shape
import Physics.World exposing (World)
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)
import Quantity
import Random
import Scene3d
import Scene3d.Material exposing (Material)
import Scene3d.Mesh
import Triangle2d
import TriangularMesh exposing (TriangularMesh)
import Vector3d exposing (Vector3d)


type Model
    = BreakableWall
        { physics : World Body
        , active : Bool
        , timeout : Float
        }



--type alias WorldCoordinates =
--    ObjCoordinates


type Body
    = WallTriangle ( Point3d Length.Meters BodyCoordinates, Scene3d.Mesh.Textured BodyCoordinates )
    | Floor


random2DPointGenerator =
    Random.pair (Random.float 0 1) (Random.float 0 1)


random2DPoints =
    Random.step
        (Random.list 20 random2DPointGenerator)
        (Random.initialSeed 777)
        |> Tuple.first
        |> List.append [ ( 0, 0 ), ( 1, 0 ), ( 0, 1 ), ( 1, 1 ) ]
        |> List.map (\( x, y ) -> Point2d.meters x y)
        |> Array.fromList



--thickness =
--    0.01


init : Float -> Model
init thickness =
    let
        vertexesWithUvs =
            DelaunayTriangulation2d.fromPoints random2DPoints
                |> Result.withDefault DelaunayTriangulation2d.empty
                |> DelaunayTriangulation2d.triangles
                |> List.map
                    (\triangle ->
                        let
                            ( p1, p2, p3 ) =
                                Triangle2d.vertices triangle

                            p1m =
                                Point2d.toMeters p1

                            p2m =
                                Point2d.toMeters p2

                            p3m =
                                Point2d.toMeters p3

                            -- Map 2D points to 3D for front face
                            p1F =
                                { x = -p1m.x, y = 0, z = p1m.y }

                            p2F =
                                { x = -p2m.x, y = 0, z = p2m.y }

                            p3F =
                                { x = -p3m.x, y = 0, z = p3m.y }

                            -- Offset by thickness for the back face
                            p1B =
                                { x = p1F.x, y = p1F.y - thickness, z = p1F.z }

                            p2B =
                                { x = p2F.x, y = p2F.y - thickness, z = p2F.z }

                            p3B =
                                { x = p3F.x, y = p3F.y - thickness, z = p3F.z }

                            frontFace =
                                ( { position = Point3d.meters p1F.x p1F.y p1F.z, uv = ( p1m.x, p1m.y ) }
                                , { position = Point3d.meters p2F.x p2F.y p2F.z, uv = ( p2m.x, p2m.y ) }
                                , { position = Point3d.meters p3F.x p3F.y p3F.z, uv = ( p3m.x, p3m.y ) }
                                )

                            backFace =
                                ( { position = Point3d.meters p1B.x p1B.y p1B.z, uv = ( p1m.x, p1m.y ) }
                                , { position = Point3d.meters p2B.x p2B.y p2B.z, uv = ( p2m.x, p2m.y ) }
                                , { position = Point3d.meters p3B.x p3B.y p3B.z, uv = ( p3m.x, p3m.y ) }
                                )

                            side1 =
                                [ ( { position = Point3d.meters p1F.x p1F.y p1F.z, uv = ( p1m.x, p1m.y ) }
                                  , { position = Point3d.meters p1B.x p1B.y p1B.z, uv = ( p1m.x, p1m.y ) }
                                  , { position = Point3d.meters p2F.x p2F.y p2F.z, uv = ( p2m.x, p2m.y ) }
                                  )
                                , ( { position = Point3d.meters p1B.x p1B.y p1B.z, uv = ( p1m.x, p1m.y ) }
                                  , { position = Point3d.meters p2B.x p2B.y p2B.z, uv = ( p2m.x, p2m.y ) }
                                  , { position = Point3d.meters p2F.x p2F.y p2F.z, uv = ( p2m.x, p2m.y ) }
                                  )
                                ]

                            side2 =
                                [ ( { position = Point3d.meters p2F.x p2F.y p2F.z, uv = ( p2m.x, p2m.y ) }
                                  , { position = Point3d.meters p2B.x p2B.y p2B.z, uv = ( p2m.x, p2m.y ) }
                                  , { position = Point3d.meters p3F.x p3F.y p3F.z, uv = ( p3m.x, p3m.y ) }
                                  )
                                , ( { position = Point3d.meters p2B.x p2B.y p2B.z, uv = ( p2m.x, p2m.y ) }
                                  , { position = Point3d.meters p3B.x p3B.y p3B.z, uv = ( p3m.x, p3m.y ) }
                                  , { position = Point3d.meters p3F.x p3F.y p3F.z, uv = ( p3m.x, p3m.y ) }
                                  )
                                ]

                            side3 =
                                [ ( { position = Point3d.meters p3F.x p3F.y p3F.z, uv = ( p3m.x, p3m.y ) }
                                  , { position = Point3d.meters p3B.x p3B.y p3B.z, uv = ( p3m.x, p3m.y ) }
                                  , { position = Point3d.meters p1F.x p1F.y p1F.z, uv = ( p1m.x, p1m.y ) }
                                  )
                                , ( { position = Point3d.meters p3B.x p3B.y p3B.z, uv = ( p3m.x, p3m.y ) }
                                  , { position = Point3d.meters p1B.x p1B.y p1B.z, uv = ( p1m.x, p1m.y ) }
                                  , { position = Point3d.meters p1F.x p1F.y p1F.z, uv = ( p1m.x, p1m.y ) }
                                  )
                                ]
                        in
                        [ frontFace
                        , backFace
                        ]
                            ++ side1
                            ++ side2
                            ++ side3
                    )

        triangles =
            vertexesWithUvs
                |> List.map
                    (\triangleVertexes ->
                        ( triangleVertexes |> List.head |> Maybe.map (\( v1, _, _ ) -> v1.position) |> Maybe.withDefault (Point3d.meters 0 0 0)
                        , TriangularMesh.triangles (List.map (\( v1, v2, v3 ) -> ( v1.position, v2.position, v3.position )) triangleVertexes)
                        , TriangularMesh.triangles triangleVertexes
                        )
                    )

        physics =
            List.foldl
                (\( position, triangleForPhysics, triangleWithUv ) world ->
                    let
                        texturedTriangle =
                            triangleWithUv
                                |> Scene3d.Mesh.texturedFacets

                        body =
                            Physics.Body.compound [ Physics.Shape.unsafeConvex triangleForPhysics ] (WallTriangle ( position, texturedTriangle ))
                                |> Physics.Body.withBehavior (Physics.Body.dynamic (Mass.kilograms 1))
                    in
                    world
                        |> Physics.World.add body
                )
                (Physics.World.add (Physics.Body.plane Floor) Physics.World.empty)
                triangles
    in
    BreakableWall
        { physics = physics
        , active = False
        , timeout = 0
        }


objectFrame : Frame3d.Frame3d Meters ObjectCoordinates { defines : WorldCoordinates }
objectFrame =
    Frame3d.atPoint (Point3d.meters 0.5 0.5 0)


view : Assets.Model -> Model -> Scene3d.Entity Coordinates.ObjectCoordinates
view assets (BreakableWall { physics }) =
    let
        material =
            Scene3d.Material.texturedNonmetal
                { baseColor = Assets.getColorTexture assets "Bricks021_1K-JPG_Color.jpg"
                , roughness = Assets.getOtherTexture assets "Bricks021_1K-JPG_Roughness.jpg"
                }
    in
    physics
        |> Physics.World.bodies
        |> List.map
            (\body ->
                let
                    frame3d =
                        Physics.Body.frame body
                in
                case Physics.Body.data body of
                    WallTriangle ( position, triangle ) ->
                        triangle
                            |> Scene3d.mesh material
                            |> Scene3d.placeIn frame3d
                            |> Scene3d.placeIn objectFrame

                    Floor ->
                        Scene3d.nothing
            )
        |> Scene3d.group


wallCenter : Point2d Length.Meters ObjectCoordinates
wallCenter =
    Point2d.meters 0.5 0.5


break : Point2d Length.Meters ObjectCoordinates -> Vector3d Length.Meters Coordinates.WorldCoordinates -> Model -> Model
break collisionPoint forceVector (BreakableWall model) =
    if model.active then
        BreakableWall model

    else
        let
            forceLength =
                forceVector
                    |> Vector3d.length
                    |> Length.inMeters

            collisionPoint3d =
                Point3d.xyz
                    (Point2d.xCoordinate collisionPoint)
                    (Length.meters 0)
                    (Point2d.yCoordinate collisionPoint)

            direction =
                forceVector
                    --|> Vector3d.plus (Vector3d.unsafe { x = 0, y = 0, z = -0.1 })
                    |> Vector3d.normalize
                    |> Vector3d.toUnitless
                    |> Direction3d.unsafe
        in
        BreakableWall
            { model
                | physics =
                    model.physics
                        |> Physics.World.withGravity (Acceleration.metersPerSecondSquared 9.80665) Direction3d.negativeZ
                        |> Physics.World.update
                            (\body ->
                                case Physics.Body.data body of
                                    WallTriangle ( position, _ ) ->
                                        let
                                            center =
                                                position
                                                    |> Point3d.toMeters
                                                    |> Point3d.fromMeters

                                            distance =
                                                Point3d.distanceFrom center collisionPoint3d

                                            impulse =
                                                Force.newtons (max 0.2 (1 - Length.inMeters distance) * forceLength * 5000)
                                                    |> Quantity.times (Duration.seconds 1)
                                        in
                                        Physics.Body.applyImpulse impulse direction center body

                                    Floor ->
                                        body
                            )
                , active = True
                , timeout = 5000
            }


update : Float -> Model -> Model
update delta (BreakableWall model) =
    if model.active then
        let
            newTimeout =
                max 0 (model.timeout - delta)
        in
        BreakableWall
            { model
                | physics =
                    if model.timeout > 0 then
                        Physics.World.simulate (Duration.milliseconds delta) model.physics

                    else
                        model.physics
                , timeout = newTimeout
            }

    else
        BreakableWall model


isBroken : Model -> Bool
isBroken (BreakableWall model) =
    model.active
