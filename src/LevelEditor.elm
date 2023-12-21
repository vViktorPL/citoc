module LevelEditor exposing (..)

import Coordinates exposing (SectorCoordinates)
import Dict exposing (Dict)
import Length
import LevelTile
import Trigger


type Model
    = LevelEditor
        { tiles : Dict SectorCoordinates ( LevelTile.Model, List Trigger.Trigger )
        , triggers : List Trigger.Trigger
        , currentTile : LevelTile.Model
        , globalTriggers : List Trigger.Trigger
        , zoom : Length.Meters
        , windowSize : ( Int, Int )
        }


type alias SectorData =
    { tile : LevelTile.Model
    , triggers : List Trigger.Trigger
    }
