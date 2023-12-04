module Coordinates exposing (ObjectCoordinates(..), SectorCoordinates, WorldCoordinates(..), sectorToWorldPosition, worldPositionToSector)

import Length exposing (Meters)
import Point3d exposing (Point3d)


type ObjectCoordinates
    = ObjectCoordinates


type WorldCoordinates
    = WorldCoordinates


type alias SectorCoordinates =
    ( Int, Int )


sectorToWorldPosition : SectorCoordinates -> Point3d Meters WorldCoordinates
sectorToWorldPosition ( sectorX, sectorY ) =
    Point3d.meters (toFloat -sectorX - 0.5) (toFloat sectorY + 0.5) 0


worldPositionToSector : Point3d Meters WorldCoordinates -> SectorCoordinates
worldPositionToSector worldPosition =
    let
        p =
            Point3d.toMeters worldPosition
    in
    ( floor -p.x, floor p.y )
