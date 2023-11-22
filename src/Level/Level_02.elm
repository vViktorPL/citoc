module Level.Level_02 exposing (data)
import Level exposing (Level, LevelTile(..), Trigger, TriggerCondition(..), TriggerEffect(..), BreakableWallType(..), fromData)
import Orientation exposing (Orientation(..))
import Color
import Length

data : Level
data = fromData [[Wall,Wall,Wall,Wall,Wall,Wall,Wall],[Wall,Floor,Floor,Floor,Floor,Floor,Wall],[Wall,Wall,Wall,Wall,Wall,Floor,Wall],[Wall,Floor,Wall,Empty,Empty,Wall],[Wall,Floor,Wall,Wall,Wall,Wall,Wall,Wall,Wall],[Wall,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Wall],[Wall,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Wall],[Wall,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Wall],[Wall,Floor,Wall,Wall,(Sign "Sign-ConfusingCorridor" South),Wall,Wall,Floor,Wall],[Wall,Floor,Wall,Floor,Floor,Floor,Wall,Floor,Wall],[Wall,Floor,Wall,Floor,Wall,Floor,Wall,Floor,Wall],[Wall,Floor,Wall,Floor,Wall,Floor,Wall,Floor,Wall],[Wall,Floor,Wall,Floor,Wall,Floor,Wall,Floor,Wall],[Wall,Floor,Floor,Floor,Wall,Floor,Floor,Floor,Wall],[Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall],[]] [Trigger (4, 9) [StepIn] [ChangeTile (1, 2) Floor, PlaySound "rumble.mp3", RemoveAllTriggersInSector (4, 9)],Trigger (5, 10) [LookAngle South] [Teleport (1, 10)],Trigger (3, 10) [LookAngle South] [Teleport (7, 10)],Trigger (4, 1) [StepIn] [NextLevel]] (1, 3) South