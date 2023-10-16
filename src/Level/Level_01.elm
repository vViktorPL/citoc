module Level.Level_01 exposing (data)
import Level exposing (Level, LevelTile(..), Trigger, TriggerCondition(..), TriggerEffect(..), fromData)
import Orientation exposing (Orientation(..))
import Color
import Length

data : Level
data = fromData [[Wall,Wall,Wall,Wall,Wall,Wall,Empty,Empty,Wall,Wall,Wall,Wall,Wall],[Wall,Floor,Wall,Floor,Floor,Wall,Empty,Empty,Wall,Floor,Floor,Floor,Wall],[Wall,Floor,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Floor,Wall,Wall,Wall,Wall],[Wall,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Wall,Floor,Floor,Floor,Floor,Wall],[Wall,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Wall,Wall,Wall,Wall,Floor,Wall],[Wall,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Wall,Empty,Empty,Wall,Floor,Wall],[Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Floor,Wall],[Empty,Empty,Empty,Empty,Empty,Empty,Wall,Floor,Wall,Floor,Wall],[Empty,Empty,Empty,Empty,Empty,Empty,Wall,Floor,Wall,Floor,Wall],[Empty,Empty,Empty,Empty,Empty,Empty,Wall,Floor,Floor,Floor,Wall],[Empty,Empty,Empty,Empty,Empty,Empty,Wall,Wall,Wall,Wall,Wall],[]] [Trigger (7, 7) [StepIn] [PlaySound "rumble.mp3", ChangeTile (9, 2) Floor, RemoveAllTriggersInSector (7, 7)],Trigger (13, 3) [StepIn] [NextLevel]] (1, 1) South