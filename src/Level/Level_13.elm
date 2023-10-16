module Level.Level_13 exposing (data)
import Level exposing (Level, LevelTile(..), Trigger, TriggerCondition(..), TriggerEffect(..), fromData)
import Orientation exposing (Orientation(..))
import Color
import Length

data : Level
data = fromData [[Wall,Floor,Wall,Empty,Wall,Floor,Wall,Empty,Wall,Floor,Wall,Empty,Wall,Wall,Wall],[Wall,Floor,Wall,Wall,Wall,Floor,Wall,Wall,Wall,Floor,Wall,Wall,Wall,Floor,Wall],[Wall,Floor,Floor,Floor,Wall,Floor,Floor,Floor,Wall,Floor,Floor,Floor,Wall,Floor,Wall,Wall,Floor,Wall],[Wall,Wall,Wall,Floor,Wall,Wall,Wall,Floor,Wall,Wall,Wall,Floor,Wall,Floor,Wall,Wall,Floor,Wall,Wall,Wall],[Wall,Wall,Wall,Floor,Wall,Wall,Wall,Floor,Wall,Wall,Wall,Floor,Wall,Floor,Wall,Wall,Floor,Floor,Floor,Wall],[Wall,Floor,Floor,Floor,Wall,Floor,Floor,Floor,Wall,Floor,Floor,Floor,Wall,Floor,Floor,Wall,Wall,Wall,Floor,Wall],[Wall,Floor,Wall,Wall,Wall,Floor,Wall,Wall,Wall,Floor,Wall,Wall,Wall,Wall,Floor,Wall,Wall,Wall,Floor,Wall],[Wall,Floor,Wall,Empty,Wall,Floor,Wall,Empty,Wall,Floor,Wall,Empty,Empty,Wall,Floor,Wall,Floor,Floor,Floor,Wall],[Wall,Floor,(Sign "Sign-1" South),Wall,Wall,Floor,(Sign "Sign-2" South),Wall,Wall,Floor,(Sign "Sign-3" South),Wall,Wall,Wall,Floor,Wall,Floor,Wall,Wall,Wall],[Wall,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Wall,Floor,Floor,Wall,Floor,Wall],[Wall,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Wall,Wall,Wall,Wall,Floor,Wall],[Wall,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Wall,Floor,Floor,Floor,Floor,Wall],[Wall,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Wall,Wall,Wall,Wall,Wall,Wall],[Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall],[Empty,Empty,Empty,Empty,Empty,Wall,Floor,Wall],[Empty,Empty,Empty,Empty,Empty,Wall,Floor,Wall],[Empty,Empty,Empty,Empty,Empty,Wall,Floor,Wall],[Empty,Empty,Empty,Empty,Empty,Wall,Floor,Wall,Wall,Wall],[Empty,Empty,Empty,Empty,Empty,Wall,Floor,Floor,Floor,Wall],[Empty,Empty,Empty,Empty,Empty,Wall,Wall,Wall,Floor,Wall],[Empty,Empty,Empty,Empty,Empty,Wall,Wall,Wall,Floor,Wall],[Empty,Empty,Empty,Empty,Empty,Wall,Floor,Floor,Floor,Wall],[Empty,Empty,Empty,Empty,Empty,Wall,Floor,Wall,Wall,Wall],[Empty,Empty,Empty,Empty,Empty,Wall,Floor,Wall],[Empty,Empty,Empty,Empty,Empty,Wall,Wall,Wall],[]] [Trigger (3, 3) [StepIn] [Teleport (8, 19), ChangeTile (6, 13) Floor, RemoveAllTriggersInSector (8, 20), CreateTrigger (Trigger (8, 20) [StepIn] [DecrementCounter "Entered_1", Teleport (3, 4)]), IncrementCounter "Entered_1"],Trigger (7, 3) [StepIn] [Teleport (8, 19), ChangeTile (6, 13) Floor, RemoveAllTriggersInSector (8, 20), CreateTrigger (Trigger (8, 20) [StepIn] [DecrementCounter "Entered_2", Teleport (7, 4)]), IncrementCounter "Entered_2"],Trigger (11, 3) [StepIn] [Teleport (8, 19), ChangeTile (6, 13) Floor, RemoveAllTriggersInSector (8, 20), CreateTrigger (Trigger (8, 20) [StepIn] [DecrementCounter "Entered_3", Teleport (11, 4)]), IncrementCounter "Entered_3"],Trigger (18, 5) [StepIn] [Teleport (8, 19), ChangeTile (6, 13) Floor, RemoveAllTriggersInSector (8, 20), CreateTrigger (Trigger (8, 20) [StepIn] [DecrementCounter "Entered_-1", Teleport (18, 6)]), IncrementCounter "Entered_-1"],Trigger (8, 18) [CounterEquals "Entered_1" 1] [ChangeTile (1, 8) BlueWall],Trigger (8, 18) [CounterEquals "Entered_2" 2] [ChangeTile (5, 8) BlueWall],Trigger (8, 18) [CounterEquals "Entered_3" 3] [ChangeTile (9, 8) BlueWall],Trigger (8, 18) [CounterEquals "Entered_-1" -1] [ChangeTile (12, 11) BlueWall, ChangeTile (12, 9) Floor],Trigger (8, 18) [CounterEquals "Entered_1" 1, CounterEquals "Entered_2" 2, CounterEquals "Entered_3" 3, CounterEquals "Entered_-1" 0] [ChangeTile (12, 12) (Sign "Sign-Minus1" West), ChangeTile (12, 11) Floor],Trigger (16, 7) [CounterEquals "Entered_-1" -1] [RemoveAllTriggersInSector (8, 19), RemoveAllTriggersInSector (8, 18), ChangeTile (12, 11) BlueWall, ChangeTile (12, 9) Floor],Trigger (14, 6) [StepIn] [NextLevel]] (6, 12) North