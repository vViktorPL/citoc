module Level.Level_01 exposing (data)
import Level exposing (Level, LevelTile(..), Trigger, TriggerCondition(..), TriggerEffect(..), BreakableWallType(..), fromData)
import Orientation exposing (Orientation(..))
import Color
import Length

data : Level
data = fromData [[Wall,Wall,Wall,Wall,Wall,Wall,Empty,Empty,Wall,Wall,Wall,Wall,Wall],[Wall,Floor,Wall,Floor,Floor,Wall,Empty,Empty,Wall,Floor,Floor,Floor,Wall],[Wall,Floor,Wall,(BreakableWall HeavyWall),Wall,Wall,Wall,Wall,Wall,Wall,Wall,Floor,Wall,Wall,Wall,Wall],[Wall,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Wall,Floor,Floor,Floor,Floor,Wall],[Wall,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Wall,Wall,Wall,Wall,Floor,Wall],[Wall,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Wall,Empty,Empty,Wall,Floor,Wall],[Wall,Wall,Wall,Floor,Wall,Wall,Wall,Wall,Wall,Floor,Wall],[Empty,Empty,BlackWall,Terms,BlackWall,Empty,Wall,Floor,Wall,Floor,Wall],[Empty,Empty,BlackWall,BlackFloor,BlackWall,Empty,Wall,Floor,Wall,Floor,Wall],[Empty,Empty,BlackWall,BlackFloor,BlackWall,Empty,Wall,Floor,Floor,Floor,Wall],[Empty,Empty,BlackWall,BlackWall,BlackWall,Empty,Wall,Wall,Wall,Wall,Wall],[]] [Trigger (7, 7) [StepIn] [PlaySound "rumble.mp3", ChangeTile (9, 2) Floor, RemoveAllTriggersInSector (7, 7)],Trigger (13, 3) [StepIn] [NextLevel],Trigger (3, 7) [StepIn] [PlayMusic "first-level.mp3", RemoveAllTriggersInSector (3, 7)],Trigger (3, 8) [Nod] [OpenTerms (3, 7), RemoveAllTriggersInSector (3, 8), RemoveAllTriggersInSector (3, 9)],Trigger (3, 9) [Nod] [OpenTerms (3, 7), RemoveAllTriggersInSector (3, 8), RemoveAllTriggersInSector (3, 9)],Trigger (3, 5) [LookAngle North] [ChangeTile (3, 6) Wall, RemoveAllTriggersInSector (3, 5)]] (3, 9) North