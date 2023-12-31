const fs = require('fs').promises;
const path = require('path');

const levelsPath = path.join(__dirname, '../levels');
const outputPath = path.join(__dirname, '../src/Level');

fs.readdir(path.join(__dirname, '../levels')).then(
  files => {
    const levelFiles = files.filter(filename => filename.split('.').pop() === "txt");
    const levelModules = levelFiles.map(levelFilename => `Level.${levelFilename.replace('.txt', '')}`);

    levelFiles.forEach(filename => {
      fs.readFile(path.join(levelsPath, filename)).then(
        content => {
          const outputFilePath = path.join(outputPath, filename.replace('.txt', '.elm'));
          const lines = String(content).split('\n');
          const legendSeparator = lines.indexOf('---');
          const rows = legendSeparator !== -1 ? lines.slice(0, legendSeparator) : lines;

          // const uniqueTilePositions = Object.fromEntries(rows.flatMap(
          //   (row, y) => [...row].map((char, x) => [char, `(${x}, ${y})`])
          // ));
          const uniqueTilePositions = {};

          rows.forEach(
            (row, y) =>
                [...row].forEach(
                  (char, x) => {
                    if (char in uniqueTilePositions) {
                      if (!Array.isArray(uniqueTilePositions[char])) {
                        uniqueTilePositions[char] = [uniqueTilePositions[char]]
                      }

                      uniqueTilePositions[char].push(`(${x}, ${y})`);
                      return;
                    }

                    uniqueTilePositions[char] = `(${x}, ${y})`;
                  }
                )
          );

          const triggers = [];

          const legend = legendSeparator !== -1 ? lines.slice(legendSeparator + 1).reduce(
            (acc, line) => {
              if (line.trim() === "") {
                return acc;
              }

              const legendData = line.substring(2)
                .replace(/@(.)/g, match => {
                  const position = uniqueTilePositions[match[1]];

                  return Array.isArray(position) ?
                    `[${position.join(',')}]` : position;
                });

              const legendTile = line.substring(0, 1);
              const levelTile = /^\(([^)]+)\)/.exec(legendData)?.[0] ?? legendData.split(' ')[0];
              const triggerData = legendData.substring(levelTile.length + 1);

              if (triggerData) {
                if (Array.isArray(uniqueTilePositions[legendTile])) {
                  uniqueTilePositions[legendTile].forEach(
                    position => {
                      triggers.push(`Trigger.localTrigger ${position} ${triggerData}`)
                    }
                  )
                } else {
                  if (triggerData.startsWith('[] ') && ['^', '>', 'v', '<'].includes(legendTile)) {
                    triggers.push(`Trigger.localTrigger ${uniqueTilePositions[legendTile]} [LevelLoaded] ${triggerData.substring(3)}`)
                  } else {
                    triggers.push(`Trigger.localTrigger ${uniqueTilePositions[legendTile]} ${triggerData}`)
                  }
                }
              }

              acc[line.substring(0, 1)] = levelTile;

              return acc;
            },
            {}
          ) : {};

          const tileRows = rows.map(
            row => `[${[...row].map(asciiToTile(legend)).join(',')}]`
          );
          const tiles = `[${tileRows.join(',')}]`;

          const startingPosY = rows.findIndex(row => /[\^><v]/.test(row));
          const { 0: orientationChar, index: startingPosX } = /[\^><v]/.exec(rows[startingPosY]);
          const startingOrientation = {
            '^': 'North',
            '>': 'East',
            'v': 'South',
            '<': 'West'
          }[orientationChar];

          const code = [
            `module Level.${filename.replace(".txt", "")} exposing (data)`,
            'import Level',
            'import LevelTile',
            'import Trigger exposing (Trigger, TriggerCondition(..), TriggerEffect(..))',
            'import Orientation exposing (Orientation(..))',
            'import Color',
            'import Length',
            '',
            `data = Level.fromData { tiles = ${tiles}, triggers = [${triggers.join(',')}], playerStartPosition = (${startingPosX}, ${startingPosY}), playerStartingOrientation = ${startingOrientation} }`,
          ].join('\n');

          fs.writeFile(outputFilePath, code);
        }
      )
    });

    const levelsData = levelModules.map(levelModule => `${levelModule}.data`);

    fs.writeFile(path.join(outputPath, "Index.elm"), [
      'module Level.Index exposing (firstLevel, restLevels)',
      '',
      'import Level',
      ...levelModules.map(levelModule => `import ${levelModule}`),
      '',
      '',
      `firstLevel = ${levelsData[0]}`,
      `restLevels = [${levelsData.slice(1).join(', ')}]`,
    ].join('\n'));
  }
);


const asciiToTile = legend => ascii => {
  if (ascii in legend) {
    const tile = legend[ascii];
    return tile[0] === '(' ? `(LevelTile.${tile.substring(1)}` : `LevelTile.${tile}`;
  }

  switch (ascii) {
    case '.':
    case '^':
    case '>':
    case 'v':
    case '<':
      return "LevelTile.floor";

    case '#':
      return "LevelTile.wall";

    default:
      return "LevelTile.empty";
  }
};
