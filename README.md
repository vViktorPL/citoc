# CITOC

![https://twitter.com/vViktorPL/status/1732893017432846799](https://pbs.twimg.com/media/GAx40ZhXYAAwu6R?format=jpg&name=4096x4096 "CITOC game screenshot")

A 3D first-person perspective puzzle game where player is constantly disorientated
and has to think outside the box written in [Elm programming language](https://elm-lang.org).

First versions was originally submitted to the [6th Elm Game Jam](https://itch.io/jam/elm-game-jam-6).

## Play

Game is deployed and playable on itch.io website:

[▶️ Play CITOC](https://vviktor.itch.io/citoc)


## Development

### Prerequisites

You have to have Node.js installed.

Install Node dependencies first:
```bash
npm i
```

### Dev mode
Run "dev" script to build levels and run dev server locally in watch mode:
```bash
npm run dev
```

### Levels

Level sources are stored inside `levels` directory. 
Levels consist of 1m² sectors. Each sector can contain 1 tile which can be one of the ones defined in `LevelTile.elm` constructors (`wall`, `floor` etc.).
The level layout is in form of an ASCII-like art where each character represents one tile from top-down view perspective on a level.
Typical tile characters are:

* ` ` (space) - empty
* `#` - wall
* `.` - floor
* `^` - a floor with player starting position, player looking angle at north
* `>` - as above, but looking angle at east
* `v` - as above, but looking angle at south
* `<` - as above, but looking angle at west

However, other types of tiles can be defined in a legend after `---` separator.

Levels consist of partitions to optimize rendering a bit. Each partition is separated by
empty row of tiles. Only tiles from partition that player is currently located in are rendered
so, they can be used mostly to create separate rooms where player is teleported into.

Such levels are transpiled into Elm source files in `src/Level` directory, so Elm
compiler can type-check theme.

#### Level editor

Levels structure is going to be refactored and improved in future so it will be possible
to load custom levels that users will be able to build by using level editor feature.

### JS code

Although game code mostly consists of Elm code, there are some parts done in JS that Elm itself couldn't handle:

* JS ⇆ Elm ports:
  * Music and sound playback
  * Textures with text generation (for signs on the walls)
  * Atypical game input handling:
    * Clipboard
    * Browser window shaking
  * LocalStorage (storage of current level number)
* Node.js:
  * `*.txt` levels to `*.elm` code transpilation script

### Build

To build the production sources that are packed into zip archive (format preferred for itch.io uploads), 
run the following npm script:
```bash
npm run build
```
