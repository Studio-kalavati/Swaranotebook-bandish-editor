import fs from 'fs';
import path from 'path';
import { createRunner, parse } from '@puppeteer/replay';
const recordingsDir = path.join(__dirname, '../puppeteer_recordings');

(async () => {
  const files = fs.readdirSync(recordingsDir).filter(f => f.endsWith('.json'));
  if (files.length === 0) {
    console.log('No replay JSON files found in recordings folder.');
    process.exit(0);
  }

  for (const file of files) {
    const recordingPath = path.join(recordingsDir, file);
    const recordingText = fs.readFileSync(recordingPath, 'utf8');
    const recording = parse(JSON.parse(recordingText));
    console.log(`Running replay for: ${file}`);

    const runner = await createRunner(recording);
    try {
      await runner.run();
      console.log(`Replay for ${file} completed successfully.`);
    } catch (err) {
      console.error(`Replay for ${file} failed:`, err);
    }
  }
})();

