const HANDWRITTEN_FONT_NAME = 'FirstTimeWriting';
const handwrittenFont = new FontFace(HANDWRITTEN_FONT_NAME, 'url(assets/FirstTimeWriting-z86Ra.ttf)');
const fontLoadPromise = handwrittenFont.load().then(function(loadedFont) {
  document.fonts.add(loadedFont);
});

const TEXTURE_RATIO = 3 / 4;
const TEXTURE_SIZE = 1024;
const [PAINT_WIDTH, PAINT_HEIGHT] =
  TEXTURE_RATIO >= 1
    ? [TEXTURE_SIZE / TEXTURE_RATIO, TEXTURE_SIZE]
    : [TEXTURE_SIZE, TEXTURE_SIZE * TEXTURE_RATIO];
const SIGN_BG_COLOR = '#eee7d7';
const INK_COLOR = '#000000';

const canvas = document.createElement('canvas');
canvas.width = TEXTURE_SIZE;
canvas.height = TEXTURE_SIZE;

const ctx = canvas.getContext('2d');

let busyCanvasPromise = Promise.resolve();

export function generateSignTexture(text) {
  busyCanvasPromise = busyCanvasPromise.then(() => generateSignTextureInternal(text));

  return busyCanvasPromise;
}

async function generateSignTextureInternal(text) {
  const textLines = text.split('\n');
  const fontSize = Math.min(PAINT_WIDTH / Math.max(...textLines.map(line => {
    let width = 0;
    for (let char of line) {
      width += char.toUpperCase() === char ? 0.75 : 0.4;
    }
    return width;
  })), PAINT_HEIGHT / (textLines.length * 1.3));

  ctx.fillStyle = SIGN_BG_COLOR;
  ctx.fillRect(0, 0, PAINT_WIDTH, PAINT_HEIGHT);

  await fontLoadPromise;

  ctx.fillStyle = INK_COLOR;
  ctx.font = `${fontSize}px ${HANDWRITTEN_FONT_NAME}`;
  ctx.textBaseline = 'top';

  const lineHeight = fontSize * (textLines.length > 1 ? 1.3 : 1);
  const textHeight = lineHeight * textLines.length;
  const textTop = PAINT_HEIGHT * 0.5 - textHeight * 0.5;

  textLines.forEach(
    (textLine, lineIndex) => {
      const textMeasurement = ctx.measureText(textLine);

      ctx.fillText(textLine, PAINT_WIDTH * 0.5 - textMeasurement.width * 0.5, textTop + lineIndex * lineHeight);
    }
  );

  ctx.drawImage(canvas, 0, 0, PAINT_WIDTH, PAINT_HEIGHT, 0, 0, TEXTURE_SIZE, TEXTURE_SIZE);

  return canvas.toDataURL('image/png');
}