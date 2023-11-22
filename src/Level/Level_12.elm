module Level.Level_12 exposing (data)
import Level exposing (Level, LevelTile(..), Trigger, TriggerCondition(..), TriggerEffect(..), BreakableWallType(..), fromData)
import Orientation exposing (Orientation(..))
import Color
import Length

data : Level
data = fromData [[Wall,Wall,Wall,Wall,Wall,Wall,Wall,(Sign "Sign-Moonwalk" South),Wall],[Wall,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Wall],[Wall,Floor,Wall,Wall,Wall,Wall,Wall,Wall,Wall],[Wall,Floor,Wall,Empty,Empty,Empty,Wall,Floor,Wall],[Wall,Wall,Wall,Empty,Empty,Empty,Wall,Floor,Wall],[Empty,Empty,Empty,Empty,Empty,Empty,Wall,Floor,Wall],[Empty,Empty,Wall,Floor,Wall,Empty,Wall,Floor,Wall],[Empty,Empty,Wall,Floor,Wall,Wall,Wall,Floor,Wall],[Empty,Empty,Wall,Floor,Floor,Floor,Floor,Floor,Wall],[Empty,Empty,Wall,Wall,Wall,Wall,Wall,Wall,Wall],[]] [Trigger (7, 1) [LookAngle North] [ChangeTile (7, 2) Floor],Trigger (7, 1) [LookAngle West] [ChangeTile (7, 2) Wall],Trigger (7, 1) [LookAngle East] [ChangeTile (7, 2) Wall],Trigger (7, 2) [StepIn] [RemoveAllTriggersInSector (7, 1)],Trigger (4, 8) [StepIn] [NextLevel]] (1, 3) North