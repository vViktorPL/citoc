module Level.Level_08 exposing (data)
import Level exposing (Level, LevelTile(..), Trigger, TriggerCondition(..), TriggerEffect(..), fromData)
import Orientation exposing (Orientation(..))
import Color
import Length

data : Level
data = fromData [[Wall,Wall,Wall,Wall],[Floor,Floor,Floor,Wall],[Wall,Wall,Floor,Wall],[Empty,Wall,Floor,Wall],[Empty,Wall,Floor,Wall],[Empty,Wall,Floor,Wall],[Empty,Wall,Floor,Wall,Wall,Wall,Wall,Wall,Wall],[Empty,Wall,Floor,Floor,Floor,Floor,Floor,Floor,Wall],[Empty,Wall,Wall,Wall,Wall,(Sign "Sign-Quiz" North),Wall,Floor,Wall],[Empty,Empty,Wall,Wall,Wall,Wall,Wall,Floor,Wall],[Empty,Empty,Wall,Floor,Floor,Floor,Wall,Floor,Wall],[Empty,Empty,Wall,Floor,Wall,Wall,Wall,Floor,Wall],[Empty,Empty,Wall,Floor,Wall,Empty,Wall,Floor,Wall,Wall,Wall,Wall,Wall,Wall],[Empty,Wall,Wall,Floor,Wall,Empty,Wall,Floor,Floor,Floor,(Sign "Sign-CorrectAnswer" West),Floor,Floor,Wall],[Empty,Floor,Floor,Floor,Wall,Empty,Wall,Wall,Wall,Wall,(Sign "Sign-CorrectAnswer" North),Wall,Floor,Wall],[Empty,Wall,Wall,Wall,Wall,Empty,Empty,Empty,Empty,Empty,Empty,Wall,Floor,Wall],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Wall,Floor,Wall],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Wall,Floor,Wall],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Wall,Floor,Wall,Wall],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Wall,Floor,Floor,Floor],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Wall,Wall,Wall,Wall]] [Trigger (7, 9) [EnteredFrom North] [Teleport (2, 3)],Trigger (2, 2) [StepIn] [Teleport (7, 8)],Trigger (12, 15) [StepIn] [Teleport (2, 3)],Trigger (5, 7) [NegativeHeadshake, LookAngle South] [ChangeTile (10, 13) Floor, RemoveAllTriggersInSector (9, 13), CreateTrigger (Trigger (9, 13) [StepIn] [CreateTrigger (Trigger (7, 9) [StepIn] [Teleport (2, 3)])]), RemoveAllTriggersInSector (7, 9)],Trigger (5, 7) [Nod, LookAngle South] [ChangeTile (10, 13) (Sign "Sign-CorrectAnswer" West), RemoveAllTriggersInSector (9, 13), CreateTrigger (Trigger (9, 13) [StepIn] [ChangeTile (7, 9) Wall, ChangeTile (6, 10) Floor]), RemoveAllTriggersInSector (7, 9)],Trigger (3, 13) [StepIn] [NextLevel]] (2, 3) South