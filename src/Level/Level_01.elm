module Level.Level_01 exposing (data)
import Level
import LevelTile
import Trigger exposing (Trigger, TriggerCondition(..), TriggerEffect(..))
import Orientation exposing (Orientation(..))
import Color
import Length

data = Level.fromData { tiles = [[LevelTile.wall,LevelTile.wall,LevelTile.wall,LevelTile.wall,LevelTile.wall,LevelTile.wall,LevelTile.wall,LevelTile.wall,LevelTile.wall],[LevelTile.wall,LevelTile.floor,LevelTile.floor,LevelTile.floor,LevelTile.floor,LevelTile.sand,LevelTile.sand,LevelTile.sand,LevelTile.wall],[LevelTile.wall,LevelTile.floor,LevelTile.floor,LevelTile.floor,LevelTile.floor,LevelTile.sand,LevelTile.sand,LevelTile.sand,LevelTile.wall],[LevelTile.wall,LevelTile.floor,LevelTile.floor,LevelTile.floor,LevelTile.floor,LevelTile.floor,LevelTile.floor,LevelTile.floor,LevelTile.wall],[LevelTile.wall,LevelTile.wall,LevelTile.wall,LevelTile.wall,LevelTile.wall,LevelTile.wall,LevelTile.wall,LevelTile.wall,LevelTile.wall],[],[]], triggers = [], playerStartPosition = (4, 3), playerStartingOrientation = North }