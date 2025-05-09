const fs = require('fs');
const path = require('path');

const recordingsDir = path.join(__dirname, 'puppeteer_recordings');
const files = fs.readdirSync(recordingsDir)
  .filter(f => f.endsWith('.json') && f !== 'index.json');

fs.writeFileSync(
  path.join(recordingsDir, 'index.json'),
  JSON.stringify(files, null, 2)
);

console.log(`Generated index.json with ${files.length} recordings`);
