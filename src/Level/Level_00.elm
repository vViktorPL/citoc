module Level.Level_00 exposing (data)
import Level exposing (Level, LevelTile(..), Trigger, TriggerCondition(..), TriggerEffect(..), fromData)
import Orientation exposing (Orientation(..))
import Color
import Length

data : Level
data = fromData [[Wall,BreakableWall,Wall],[Wall,Floor,Wall],[Wall,Floor,Wall],[Wall,Floor,Wall],[Wall,BreakableWall,Wall],[Wall,Floor,Wall],[Wall,Floor,Wall],[Wall,Floor,Wall],[Wall,Floor,Wall],[Wall,BreakableWall,Wall],[Wall,Floor,Wall],[Wall,Floor,Wall],[Wall,Floor,Wall],[Wall,Floor,Wall],[Wall,BreakableWall,Wall],[Wall,Floor,Wall],[Wall,Floor,Wall],[Wall,Floor,Wall],[Wall,Floor,Wall],[Wall,BreakableWall,Wall],[Wall,Floor,Wall],[Wall,Floor,Wall],[Wall,Floor,Wall],[Wall,Floor,Wall],[Wall,BreakableWall,Wall],[Wall,Floor,Wall],[Wall,Floor,Wall],[Wall,Floor,Wall],[Wall,Floor,Wall],[Wall,BreakableWall,Wall],[Wall,Floor,Wall],[Wall,Floor,Wall],[Wall,Wall,Wall]] [] (1, 31) North