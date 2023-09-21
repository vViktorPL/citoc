module Level.Index exposing (firstLevel, restLevels)

import Level exposing (Level)
import Level.Level_01
import Level.Level_02
import Level.Level_04
import Level.Level_08
import Level.Level_10
import Level.Level_11


firstLevel : Level
firstLevel = Level.Level_01.data

restLevels : List Level
restLevels = [Level.Level_02.data, Level.Level_04.data, Level.Level_08.data, Level.Level_10.data, Level.Level_11.data]