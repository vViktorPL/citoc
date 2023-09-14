module Level.Level_00 exposing (data)
import Level exposing (Level, LevelTile(..), Orientation(..), Trigger, TriggerCondition(..), TriggerEffect(..), fromData)

data : Level
data = fromData [[Wall,Wall,Wall,Empty,Empty,Empty,Empty,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall],[Wall,Floor,Wall,Empty,Empty,Empty,Empty,Wall,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Wall,Floor,Floor,Floor,Floor,Wall,Empty,Empty,Empty,Wall,Wall,Wall],[Wall,Floor,Wall,Wall,Wall,Wall,Wall,Wall,Floor,Floor,Floor,Floor,Floor,Floor,Floor,(Sign "Sign1.png" West),Floor,Floor,Wall,Floor,Wall,Floor,Floor,Wall,Empty,Empty,Empty,Wall,Floor,Wall],[Wall,Floor,Wall,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Wall,Floor,Wall,Floor,Floor,Wall,Empty,Empty,Empty,Wall,Floor,Wall],[Wall,Floor,Wall,Floor,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,(Sign "Sign7.png" West),Wall,Wall,Wall,Wall,Wall,(Sign "Sign3.png" North),Wall,Floor,Floor,Wall,Empty,Wall,Wall,Wall,Floor,Wall],[Wall,Floor,Floor,Floor,Wall,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,(Sign "Sign2.png" East),Floor,Floor,Floor,Floor,Floor,Wall,Empty,(Sign "Sign2.png" East),Floor,Floor,Floor,Wall],[Wall,Wall,Wall,Wall,Wall,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Wall,Floor,Floor,Floor,Floor,(Sign "Sign5.png" West)],[Wall,Wall,Wall,Empty,Empty,Empty,Empty,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Floor,Wall,(Sign "Sign4.png" South),Wall,Wall,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Empty,Empty,Wall,Wall,Wall],[Wall,Floor,Wall,Empty,Empty,Empty,Empty,Wall,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Wall,Floor,Floor,Floor,Floor,Wall,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Wall,Floor,Floor,Floor,Floor,Floor,(Sign "Sign8.png" West),Empty,Empty,Wall,Floor,(Sign "Sign8.png" West)],[Wall,Floor,Wall,Wall,Wall,Wall,Wall,Wall,Floor,Floor,Floor,Floor,Floor,Floor,Floor,(Sign "Sign1.png" West),Floor,Floor,Wall,Floor,Wall,Floor,Floor,Wall,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Wall,Floor,Wall,Wall,Wall,Wall,Wall,Empty,Empty,Wall,Wall,Wall],[Wall,Floor,Wall,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Wall,Floor,Floor,Floor,Floor,Wall,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Wall,Floor,Floor,Floor,Floor,Floor,(Sign "Sign7.png" West)],[Wall,Floor,Wall,Floor,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Wall,Wall,Wall,Wall,Wall,Wall,Wall],[Wall,Floor,Floor,Floor,Wall],[Wall,Wall,Wall,Wall,Wall,Empty,Empty,Empty,Wall,Wall,Wall,Empty,Empty,Empty,Empty,Wall,Wall,Wall],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,(Sign "Sign6.png" East),Floor,(Sign "Sign5.png" West),Empty,Empty,Empty,Empty,(Sign "Sign6.png" East),Floor,(Sign "Sign7.png" West)],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Wall,Wall,Wall,Empty,Empty,Empty,Empty,Wall,Wall,Wall]] [Trigger (15, 1) [EnteredFrom West, LookAngle East] [Teleport (20, 1)],Trigger (20, 1) [EnteredFrom East, LookAngle East] [Teleport (15, 1)],Trigger (18, 5) [LookAngle West] [Teleport (26, 5)],Trigger (28, 2) [LookAngle North] [Teleport (1, 9)],Trigger (15, 11) [EnteredFrom West, LookAngle East] [Teleport (20, 11)],Trigger (22, 7) [EnteredFrom West, LookAngle East] [Teleport (9, 15)],Trigger (9, 15) [LookAngle West] [Teleport (16, 15)],Trigger (16, 15) [LookAngle East] [Teleport (36, 11)],Trigger (36, 9) [LookAngle East] [Teleport (41, 9)],Trigger (41, 9) [LookAngle West] [Teleport (1, 1)]] (1, 1) South