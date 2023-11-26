module Level.Index exposing (firstLevel, restLevels)

import Level exposing (Level)
import Level.Level_01
import Level.Level_02
import Level.Level_03
import Level.Level_04
import Level.Level_05
import Level.Level_06
import Level.Level_07
import Level.Level_08
import Level.Level_09


firstLevel : Level
firstLevel = Level.Level_01.data

restLevels : List Level
restLevels = [Level.Level_02.data, Level.Level_03.data, Level.Level_04.data, Level.Level_05.data, Level.Level_06.data, Level.Level_07.data, Level.Level_08.data, Level.Level_09.data]