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

          const uniqueTilePositions = Object.fromEntries(rows.flatMap(
            (row, y) => [...row].map((char, x) => [char, `(${x}, ${y})`])
          ));

          const triggers = [];

          const legend = legendSeparator !== -1 ? lines.slice(legendSeparator + 1).reduce(
            (acc, line) => {
              if (line.trim() === "") {
                return acc;
              }

              const legendData = line.substring(2).replace(/@(.)/g, match => uniqueTilePositions[match[1]]);

              const legendTile = line.substring(0, 1);
              const levelTile = /^\(([^)]+)\)/.exec(legendData)?.[0] ?? legendData.split(' ')[0];
              const triggerData = legendData.substring(levelTile.length + 1);

              if (triggerData) {
                triggers.push(`Trigger ${uniqueTilePositions[legendTile]} ${triggerData}`)
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
            'import Level exposing (Level, LevelTile(..), Trigger, TriggerCondition(..), TriggerEffect(..), fromData)',
            'import Orientation exposing (Orientation(..))',
            'import Color',
            'import Length',
            // 'import Screen.Game.Direction exposing (..)',
            // 'import Color exposing (Color)',
            '',
            'data : Level',
            `data = fromData ${tiles} [${triggers.join(',')}] (${startingPosX}, ${startingPosY}) ${startingOrientation}`,
          ].join('\n');

          fs.writeFile(outputFilePath, code);
        }
      )
    });

    const levelsData = levelModules.map(levelModule => `${levelModule}.data`);

    fs.writeFile(path.join(outputPath, "Index.elm"), [
      'module Level.Index exposing (firstLevel, restLevels)',
      '',
      'import Level exposing (Level)',
      ...levelModules.map(levelModule => `import ${levelModule}`),
      '',
      '',
      'firstLevel : Level',
      `firstLevel = ${levelsData[0]}`,
      '',
      'restLevels : List Level',
      `restLevels = [${levelsData.slice(1).join(', ')}]`,
    ].join('\n'));
  }
);


const asciiToTile = legend => ascii => {
  if (ascii in legend) {
    return legend[ascii];
  }

  switch (ascii) {
    case '.':
    case '^':
    case '>':
    case 'v':
    case '<':
      return "Floor";

    case '#':
      return "Wall";

    default:
      return "Empty";
  }
};
