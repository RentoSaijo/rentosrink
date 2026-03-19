import fs from "node:fs/promises";
import path from "node:path";
import os from "node:os";
import { spawn } from "node:child_process";
import puppeteer from "puppeteer-core";
import ffmpegPath from "ffmpeg-static";

const chromePath = "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome";

function parseSeconds(text) {
  if (!text) {
    return 0;
  }

  const match = /^([0-9]*\.?[0-9]+)s$/.exec(text.trim());
  return match ? Number(match[1]) : 0;
}

function inferDuration(svgText) {
  const animateTags = [...svgText.matchAll(/<animate\b[^>]*begin="([^"]+)"[^>]*dur="([^"]+)"/g)];

  if (animateTags.length === 0) {
    return 4;
  }

  let maxEnd = 0;
  for (const [, beginRaw, durRaw] of animateTags) {
    const begin = parseSeconds(beginRaw);
    const dur = parseSeconds(durRaw);
    maxEnd = Math.max(maxEnd, begin + dur);
  }

  return maxEnd + 0.4;
}

function runCommand(cmd, args, opts = {}) {
  return new Promise((resolve, reject) => {
    const proc = spawn(cmd, args, { stdio: "pipe", ...opts });
    let stderr = "";

    proc.stderr.on("data", chunk => {
      stderr += chunk.toString();
    });

    proc.on("error", reject);
    proc.on("close", code => {
      if (code === 0) {
        resolve();
      } else {
        reject(new Error(stderr || `Command failed with exit code ${code}`));
      }
    });
  });
}

async function main() {
  const inputArg = process.argv[2];
  if (!inputArg) {
    throw new Error("Usage: node models/passes/export_svg_mp4.mjs /absolute/path/to/file.svg");
  }

  const inputPath = path.resolve(inputArg);
  const outputPath = inputPath.replace(/\.svg$/i, ".mp4");
  if (outputPath === inputPath) {
    throw new Error("Input file must have .svg extension.");
  }

  const svgText = await fs.readFile(inputPath, "utf8");
  const durationSec = inferDuration(svgText);
  const fps = 30;
  const totalFrames = Math.max(1, Math.ceil(durationSec * fps));

  const tempDir = await fs.mkdtemp(path.join(os.tmpdir(), "svg-mp4-"));
  const browser = await puppeteer.launch({
    executablePath: chromePath,
    headless: "new",
    args: ["--allow-file-access-from-files"]
  });

  try {
    const page = await browser.newPage();
    await page.setViewport({ width: 1400, height: 900, deviceScaleFactor: 1 });
    await page.goto(`file://${inputPath}`, { waitUntil: "load" });

    await page.evaluate(() => {
      const root = document.documentElement;
      if (typeof root.pauseAnimations === "function") {
        root.pauseAnimations();
        root.setCurrentTime(0);
      }
    });

    for (let frameIdx = 0; frameIdx < totalFrames; frameIdx += 1) {
      const timeSec = frameIdx / fps;
      await page.evaluate(time => {
        const root = document.documentElement;
        if (typeof root.setCurrentTime === "function") {
          root.setCurrentTime(time);
        }
      }, timeSec);

      const framePath = path.join(tempDir, `frame-${String(frameIdx).padStart(5, "0")}.png`);
      await page.screenshot({ path: framePath });
    }

    await runCommand(ffmpegPath, [
      "-y",
      "-framerate",
      String(fps),
      "-i",
      path.join(tempDir, "frame-%05d.png"),
      "-c:v",
      "libx264",
      "-pix_fmt",
      "yuv420p",
      "-movflags",
      "+faststart",
      outputPath
    ]);
  } finally {
    await browser.close();
    await fs.rm(tempDir, { recursive: true, force: true });
  }

  console.log(`Saved ${outputPath}`);
}

main().catch(err => {
  console.error(err.message);
  process.exit(1);
});
