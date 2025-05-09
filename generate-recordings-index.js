const fs = require('fs');
const path = require('path');

const recordingsDir = path.join(__dirname, 'puppeteer_recordings');

// Create directory if it doesn't exist
if (!fs.existsSync(recordingsDir)) {
  fs.mkdirSync(recordingsDir, { recursive: true });
  console.log(`Created directory: ${recordingsDir}`);
}

// Get all JSON files except index.json
const files = fs.readdirSync(recordingsDir)
  .filter(f => f.endsWith('.json') && f !== 'index.json');

// Write the index file
fs.writeFileSync(
  path.join(recordingsDir, 'index.json'),
  JSON.stringify(files, null, 2)
);

console.log(`Generated index.json with ${files.length} recordings`);
