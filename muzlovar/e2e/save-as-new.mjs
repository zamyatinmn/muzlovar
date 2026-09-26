// Headless E2E для Muzlovar: «Сохранить как новую».
//
// Сценарии:
//   1. Новая подборка публикуется обычной кнопкой «Опубликовать»;
//   2. Открыта опубликованная A: изменение названия и условия, затем
//      «Сохранить как новую» — create без identity A: существуют A и B,
//      A побайтово прежняя, B содержит изменения, state содержит обе
//      identity, редактор переключился на B;
//   3. Изменение B → «Сохранить изменения» — меняется B, A побайтово
//      прежняя;
//   4. Конфликт имени новой копии: 409, диалог отменён — A, B и state
//      не тронуты, редактор остался на A.
//
// Запуск:  node save-as-new.mjs   (или npm run test:save-as-new)
// Переменные:
//   MUZLOVAR_E2E_SKIP_BUILD=1  — не запускать cabal build (exe уже собран)
//   MUZLOVAR_E2E_CHROME=<путь> — свой путь к chrome/chromium.exe

import { spawn, spawnSync } from "node:child_process";
import {
  existsSync,
  mkdtempSync,
  mkdirSync,
  readdirSync,
  readFileSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import net from "node:net";
import { tmpdir } from "node:os";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { chromium } from "playwright-core";

const here = path.dirname(fileURLToPath(import.meta.url));
const repoRoot = path.resolve(here, "..", "..");

// ---------------------------------------------------------------- helpers

const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

function assert(cond, msg) {
  if (!cond) throw new Error(msg);
}

function assertEq(actual, expected, msg) {
  assert(
    actual === expected,
    msg + ": ожидалось " + JSON.stringify(expected) + ", фактически " + JSON.stringify(actual),
  );
}

async function waitForServer(url, timeoutMs = 30000) {
  const start = Date.now();
  while (Date.now() - start < timeoutMs) {
    try {
      const res = await fetch(url);
      if (res.ok) return;
    } catch {
      /* not up yet */
    }
    await sleep(250);
  }
  throw new Error("server did not start in time: " + url);
}

function findChrome() {
  if (process.env.MUZLOVAR_E2E_CHROME) return process.env.MUZLOVAR_E2E_CHROME;
  const localAppData = process.env.LOCALAPPDATA;
  if (!localAppData) return null;
  const base = path.join(localAppData, "ms-playwright");
  try {
    for (const dir of readdirSync(base)) {
      if (!dir.startsWith("chromium")) continue;
      for (const sub of ["chrome-win64", "chrome-win"]) {
        const c = path.join(base, dir, sub, dir.startsWith("chromium_headless") ? "headless_shell.exe" : "chrome.exe");
        if (existsSync(c)) return c;
      }
    }
  } catch {
    /* no ms-playwright dir */
  }
  return null;
}

// Склейка каталога и имени файла — как publishFilePath в muzlovar.js.
function joinLike(dir, name) {
  const trimmed = dir.replace(/[\\\/]+$/, "");
  if (!trimmed) return name;
  const sep = trimmed.indexOf("\\") >= 0 && trimmed.indexOf("/") < 0 ? "\\" : "/";
  return trimmed + sep + name;
}

// Атрибуты редактора и текст блока «Путь публикации”.
function pathState(page) {
  return page.$eval("#editor", (ed) => ({
    dir: ed.getAttribute("data-publish-dir") || "",
    current: ed.getAttribute("data-published-path") || "",
    published: ed.getAttribute("data-published"),
    slug: ed.getAttribute("data-slug"),
    text: document.getElementById("e-path").textContent,
  }));
}

async function waitPathText(page, expected) {
  await page.waitForFunction(
    (t) => {
      const p = document.getElementById("e-path");
      return p && p.textContent === t;
    },
    expected,
    { timeout: 15000 },
  );
}

// Подписи и видимость кнопок публикации в шапке редактора.
function publishButtons(page) {
  return page.$eval("#editor", () => {
    const main = document.getElementById("e-publish");
    const copy = document.getElementById("e-publish-new");
    return {
      main: main ? main.textContent.trim() : null,
      asNewVisible: !!copy && !copy.hidden,
      asNewDisabled: !!copy ? copy.disabled : null,
    };
  });
}

// Рецепт можно публиковать: проверка завершилась без ошибок
// (предупреждения публикацию не блокируют — см. muzlovar.js).
async function waitPublishable(page) {
  await page.waitForFunction(
    () => {
      const b = document.getElementById("e-validity");
      return b && (b.className === "validity ok" || b.className === "validity warn");
    },
    null,
    { timeout: 20000 },
  );
}

// Дождаться следующего ответа на POST /api/validate.
function nextValidate(page) {
  return page.waitForResponse(
    (r) => r.url().indexOf("/api/validate") >= 0 && r.request().method() === "POST",
  );
}

// Изменить рецепт и дождаться завершившейся (успешной) проверки.
async function applyEdit(page, fn) {
  const v = nextValidate(page);
  await fn();
  const res = await v;
  assert(res.ok(), "validate после изменения: HTTP " + res.status());
  await waitPublishable(page);
}

// Дождаться ответа на публикацию (PUT/POST /api/playlists[...]).
function nextPublish(page, method, urlPart) {
  return page.waitForResponse(
    (r) =>
      r.request().method() === method &&
      r.url().indexOf("/api/playlists") >= 0 &&
      (!urlPart || r.url().indexOf(urlPart) >= 0),
  );
}

// Путь без query-string — для проверки, что create ушёл без identity.
function requestPath(res) {
  const u = new URL(res.request().url());
  return u.pathname;
}

async function waitDialog(page, open) {
  await page.waitForFunction(
    (want) => {
      const d = document.getElementById("overwrite-dialog");
      return !!d && d.open === want;
    },
    open,
    { timeout: 5000 },
  );
}

function readState() {
  const p = path.join(rulesDir, ".muzlovar-published.json");
  if (!existsSync(p)) return null;
  return JSON.parse(readFileSync(p, "utf8"));
}

function recordOf(slug) {
  const state = readState();
  if (!state || !Array.isArray(state.playlists)) return null;
  const r = state.playlists.find((p) => p.slug === slug);
  return r ? JSON.stringify(r) : null;
}

function stateSlugs() {
  const state = readState();
  if (!state || !Array.isArray(state.playlists)) return [];
  return state.playlists.map((p) => p.slug).sort();
}

// ---------------------------------------------------------------- locations

let store = null;
let rulesDir = null;
let playlistsDir = null;

// Снимки содержимого для побайтовых проверок «ничего не тронуто».
let snapA = null;   // { nsp, mix } опубликованной A
let alphaRecord = null; // запись state об A после её публикации

const nspOf = (slug) => path.join(playlistsDir, slug + ".nsp");
const mixOf = (slug) => path.join(rulesDir, slug + ".mix");

const bytes = (p) => readFileSync(p);
const snap = (slug) => ({ nsp: bytes(nspOf(slug)), mix: bytes(mixOf(slug)) });

function assertUnchanged(slug, before, label) {
  assert(existsSync(nspOf(slug)), label + ": нет " + slug + ".nsp");
  assert(existsSync(mixOf(slug)), label + ": нет " + slug + ".mix");
  assert(
    Buffer.compare(bytes(nspOf(slug)), before.nsp) === 0,
    label + ": " + slug + ".nsp изменился",
  );
  assert(
    Buffer.compare(bytes(mixOf(slug)), before.mix) === 0,
    label + ": " + slug + ".mix изменился",
  );
}

// Записи POST/PUT /api/playlists — для проверки, что overwrite не шёл.
let publishRequests = [];

// ---------------------------------------------------------------- scenarios

async function scenario1_publishDraft(page) {
  await page.goto(baseUrl + "/new", { waitUntil: "networkidle" });
  await page.waitForSelector('.ing[data-chip-field="год"]', { timeout: 15000 });

  // Черновик: основная кнопка «Опубликовать», «Сохранить как новую» скрыта.
  let btn = await publishButtons(page);
  assertEq(btn.main, "Опубликовать", "подпись основной кнопки черновика");
  assertEq(btn.asNewVisible, false, "«Сохранить как новую» видна черновику");

  // Название + одно условие — иначе пустое дерево не валидно.
  await applyEdit(page, async () => {
    await page.fill("#e-name", "SaveAsNew Alpha");
    await page.click('.ing[data-chip-field="год"]');
  });

  const st = await pathState(page);
  assertEq(st.published, "0", "data-published до первой публикации");

  const w = nextPublish(page, "POST");
  await page.click("#e-publish");
  const res = await w;
  assertEq(res.status(), 201, "статус первой публикации");
  assertEq(requestPath(res), "/api/playlists", "create черновика");

  // Файлы и state созданы.
  assert(existsSync(nspOf("saveasnew-alpha")), "нет saveasnew-alpha.nsp");
  assert(existsSync(mixOf("saveasnew-alpha")), "нет saveasnew-alpha.mix");
  const state = readState();
  assertEq(state.playlists.length, 1, "state после первой публикации");
  assertEq(state.playlists[0].slug, "saveasnew-alpha", "slug в state");

  // Путь публикации и переключение кнопок после успеха.
  await waitPathText(page, joinLike(st.dir, "saveasnew-alpha.nsp"));
  await page.waitForFunction(
    () => {
      const main = document.getElementById("e-publish");
      const copy = document.getElementById("e-publish-new");
      return (
        !!main &&
        main.textContent.trim() === "Сохранить изменения" &&
        !!copy &&
        !copy.hidden
      );
    },
    null,
    { timeout: 5000 },
  );
  const after = await pathState(page);
  assertEq(after.published, "1", "data-published после публикации");
  assertEq(after.slug, "saveasnew-alpha", "data-slug после публикации");
  assert(
    page.url().indexOf("/edit/saveasnew-alpha") >= 0,
    "URL не переехал: " + page.url(),
  );

  snapA = snap("saveasnew-alpha");
  alphaRecord = recordOf("saveasnew-alpha");
  assert(alphaRecord !== null, "нет записи state об A");
}

async function scenario2_saveAsNew(page) {
  // Свежая загрузка страницы A: кнопки рендерит сервер.
  await page.goto(baseUrl + "/edit/saveasnew-alpha", { waitUntil: "networkidle" });
  await page.waitForSelector("#e-path", { timeout: 15000 });
  await waitPublishable(page);

  const st0 = await pathState(page);
  assertEq(st0.slug, "saveasnew-alpha", "открыт slug A");
  assertEq(st0.published, "1", "A опубликована");
  const btn = await publishButtons(page);
  assertEq(btn.main, "Сохранить изменения", "подпись основной кнопки опубликованной");
  assertEq(btn.asNewVisible, true, "«Сохранить как новую» скрыта у опубликованной");

  // Изменить название и условие.
  await applyEdit(page, async () => {
    await page.fill("#e-name", "SaveAsNew Beta");
    await page.click('.ing[data-chip-field="оценка"]');
  });

  const w = nextPublish(page, "POST");
  await page.click("#e-publish-new");
  const res = await w;
  assertEq(res.status(), 201, "статус «Сохранить как новую»");
  // НЕ передаются identity/previous slug исходной подборки.
  assertEq(requestPath(res), "/api/playlists", "в create ушёл identity: " + res.request().url());

  // Обе подборки существуют; A не тронута (нет cleanup/rename).
  assert(existsSync(nspOf("saveasnew-beta")), "нет saveasnew-beta.nsp");
  assert(existsSync(mixOf("saveasnew-beta")), "нет saveasnew-beta.mix");
  assertUnchanged("saveasnew-alpha", snapA, "после «Сохранить как новую»");

  // B содержит изменения: название и новое условие.
  const betaNsp = bytes(nspOf("saveasnew-beta")).toString("utf8");
  const betaMix = bytes(mixOf("saveasnew-beta")).toString("utf8");
  assert(betaNsp.indexOf("SaveAsNew Beta") >= 0, "в B нет нового названия");
  assert(betaMix.indexOf("оценка") >= 0, "в B нет нового условия");
  assert(
    Buffer.compare(bytes(nspOf("saveasnew-beta")), snapA.nsp) !== 0,
    "B.nsp совпала с A.nsp",
  );
  assert(
    Buffer.compare(bytes(mixOf("saveasnew-beta")), snapA.mix) !== 0,
    "B.mix совпала с A.mix",
  );

  // A осталась прежней: название, файлы и её запись в state.
  const alphaNsp = bytes(nspOf("saveasnew-alpha")).toString("utf8");
  assert(alphaNsp.indexOf("SaveAsNew Alpha") >= 0, "название A изменилось");
  assertEq(recordOf("saveasnew-alpha"), alphaRecord, "запись state об A изменилась");

  // State содержит обе identity.
  assertEq(
    JSON.stringify(stateSlugs()),
    JSON.stringify(["saveasnew-alpha", "saveasnew-beta"]),
    "identity в state после save-as-new",
  );
  const state = readState();
  assertEq(state.playlists.length, 2, "записей в state после save-as-new");

  // Редактор переключился на identity новой подборки.
  await waitPathText(page, joinLike(st0.dir, "saveasnew-beta.nsp"));
  const st = await pathState(page);
  assertEq(st.slug, "saveasnew-beta", "data-slug после save-as-new");
  assertEq(st.published, "1", "data-published после save-as-new");
  assert(
    page.url().indexOf("/edit/saveasnew-beta") >= 0,
    "URL после save-as-new: " + page.url(),
  );
  const btnAfter = await publishButtons(page);
  assertEq(btnAfter.main, "Сохранить изменения", "подпись кнопки у новой подборки");

  // A доступна по API и не изменилась.
  const a = await fetch(baseUrl + "/api/playlists/saveasnew-alpha");
  assertEq(a.status, 200, "A доступна после save-as-new");
  const aBody = await a.json();
  assertEq(aBody.playlist.name, "SaveAsNew Alpha", "название A по API");
}

async function scenario3_updateB(page) {
  // Редактор открыт на B (после save-as-new).
  const st0 = await pathState(page);
  assertEq(st0.slug, "saveasnew-beta", "открыт slug B");
  const betaBefore = snap("saveasnew-beta");

  // Изменить условие B: filename не меняется → обычный update.
  await applyEdit(page, async () => {
    await page.click('.ing[data-chip-field="обложка"]');
  });

  const conflict = nextPublish(page, "PUT");
  await page.click("#e-publish");
  const cRes = await conflict;
  assertEq(cRes.status(), 409, "update без overwrite на неизменном filename");
  await waitDialog(page, true);

  const w = nextPublish(page, "PUT", "overwrite=1");
  await page.click("#overwrite-dialog button.primary");
  const res = await w;
  assertEq(res.status(), 200, "статус «Сохранить изменения»");
  await waitDialog(page, false);

  // B изменилась, A побайтово прежняя.
  const betaAfter = snap("saveasnew-beta");
  assert(
    Buffer.compare(betaAfter.nsp, betaBefore.nsp) !== 0,
    "B.nsp не изменилась после update",
  );
  assert(
    Buffer.compare(betaAfter.mix, betaBefore.mix) !== 0,
    "B.mix не изменилась после update",
  );
  assertUnchanged("saveasnew-alpha", snapA, "после «Сохранить изменения» B");

  // State по-прежнему содержит обе identity, запись A не тронута.
  assertEq(
    JSON.stringify(stateSlugs()),
    JSON.stringify(["saveasnew-alpha", "saveasnew-beta"]),
    "identity в state после update B",
  );
  assertEq(recordOf("saveasnew-alpha"), alphaRecord, "запись state об A после update B");
  const betaRecord = recordOf("saveasnew-beta");
  assert(betaRecord !== null, "нет записи state об B после update");
  assert(
    betaRecord.indexOf("saveasnew-beta.nsp") >= 0,
    "путь .nsp в записи B не сохранён: " + betaRecord,
  );

  const st = await pathState(page);
  assertEq(st.slug, "saveasnew-beta", "slug B после update");
}

async function scenario4_copyNameConflict(page) {
  // Открыть A и попытаться сохранить её копию под тем же именем.
  await page.goto(baseUrl + "/edit/saveasnew-alpha", { waitUntil: "networkidle" });
  await page.waitForSelector("#e-path", { timeout: 15000 });
  await waitPublishable(page);

  const before = {
    alpha: snap("saveasnew-alpha"),
    beta: snap("saveasnew-beta"),
    state: JSON.stringify(readState()),
  };
  const reqsBefore = publishRequests.length;

  const w = nextPublish(page, "POST");
  await page.click("#e-publish-new");
  const res = await w;
  assertEq(res.status(), 409, "конфликт имени новой копии");
  assertEq(requestPath(res), "/api/playlists", "конфликт пришёл не в create");
  await waitDialog(page, true);

  // Отмена: перезапись не запрошена, ничего не изменено.
  await page.click("#overwrite-dialog button:not(.primary)");
  await waitDialog(page, false);

  const extra = publishRequests.slice(reqsBefore);
  assert(
    extra.every((r) => r.indexOf("overwrite=1") < 0),
    "после отмены ушёл overwrite: " + JSON.stringify(extra),
  );
  assert(
    extra.length === 1,
    "лишние запросы публикации при конфликте: " + JSON.stringify(extra),
  );

  assertUnchanged("saveasnew-alpha", before.alpha, "при конфликте имени копии");
  assertUnchanged("saveasnew-beta", before.beta, "при конфликте имени копии");
  assertEq(JSON.stringify(readState()), before.state, "state изменился при конфликте");

  // Исходная A осталась открытой и опубликованной.
  const st = await pathState(page);
  assertEq(st.slug, "saveasnew-alpha", "slug после отмены конфликта");
  assertEq(st.published, "1", "data-published после отмены конфликта");
  assert(
    page.url().indexOf("/edit/saveasnew-alpha") >= 0,
    "URL после отмены конфликта: " + page.url(),
  );
  const btn = await publishButtons(page);
  assertEq(btn.main, "Сохранить изменения", "подпись основной кнопки после отмены");
}

const scenarios = [
  ["1. черновик публикуется кнопкой «Опубликовать»", scenario1_publishDraft],
  ["2. «Сохранить как новую»: A и B, A не тронута", scenario2_saveAsNew],
  ["3. «Сохранить изменения» B: меняется B, A прежняя", scenario3_updateB],
  ["4. конфликт имени копии: A не затронута", scenario4_copyNameConflict],
];

// ---------------------------------------------------------------- main

async function main() {
  // 1. build
  if (!process.env.MUZLOVAR_E2E_SKIP_BUILD) {
    console.log("[e2e-saveasnew] cabal build exe:muzlovar ...");
    const r = spawnSync("cabal build exe:muzlovar", {
      cwd: repoRoot,
      stdio: "inherit",
      shell: true,
    });
    if (r.status !== 0) {
      console.error(
        "[e2e-saveasnew] cabal build failed — если ошибка «Permission denied» на muzlovar.exe, " +
          "закройте запущенный сервер и повторите (или задайте MUZLOVAR_E2E_SKIP_BUILD=1).",
      );
      process.exit(1);
    }
  }
  const lb = spawnSync("cabal list-bin exe:muzlovar", {
    cwd: repoRoot,
    shell: true,
    encoding: "utf8",
  });
  const exePath = (lb.stdout || "").trim();
  if (lb.status !== 0 || !exePath || !existsSync(exePath)) {
    throw new Error("cabal list-bin exe:muzlovar failed: " + (lb.stdout || lb.stderr || ""));
  }

  // 2. temp store (seed-файлов нет: всё создаётся через UI)
  store = mkdtempSync(path.join(tmpdir(), "muzlovar-e2e-saveasnew-"));
  rulesDir = path.join(store, "rules");
  playlistsDir = path.join(store, "playlists");
  mkdirSync(rulesDir, { recursive: true });

  // 3. free port + server
  const port = await new Promise((resolve, reject) => {
    const srv = net.createServer();
    srv.listen(0, "127.0.0.1", () => {
      const p = srv.address().port;
      srv.close(() => resolve(p));
    });
    srv.on("error", reject);
  });
  baseUrl = "http://127.0.0.1:" + port;
  console.log("[e2e-saveasnew] exe:  " + exePath);
  console.log("[e2e-saveasnew] store:" + store);
  console.log("[e2e-saveasnew] url:  " + baseUrl);
  const server = spawn(exePath, [], {
    env: {
      ...process.env,
      MUZLOVAR_USERNAME: "",
      MUZLOVAR_PASSWORD: "",
      MUZLOVAR_HOST: "127.0.0.1",
      MUZLOVAR_PORT: String(port),
      MUZLOVAR_RULES_DIR: rulesDir,
      MUZLOVAR_PLAYLISTS_DIR: playlistsDir,
      MUZLOVAR_TRASH_DIR: path.join(store, "trash"),
    },
    cwd: store,
    stdio: ["ignore", "pipe", "pipe"],
    shell: false,
  });
  server.stdout.on("data", (d) => process.stdout.write("[srv] " + d));
  server.stderr.on("data", (d) => process.stderr.write("[srv] " + d));
  let serverDead = false;
  server.on("exit", (code) => {
    serverDead = true;
    if (code !== 0 && code !== null) console.error("[e2e-saveasnew] server exited with " + code);
  });

  let failed = 0;
  let browser;
  let page;
  try {
    await waitForServer(baseUrl + "/health");

    const chrome = findChrome();
    if (!chrome) throw new Error("chrome not found; set MUZLOVAR_E2E_CHROME");
    browser = await chromium.launch({ executablePath: chrome, headless: true });
    const context = await browser.newContext({ viewport: { width: 1400, height: 900 } });
    await context.addInitScript(() => localStorage.setItem('muzlovar.locale', 'ru'));
    page = await context.newPage();
    page.on("pageerror", (e) => console.error("[e2e-saveasnew] pageerror: " + e.message));
    publishRequests = [];
    page.on("request", (r) => {
      const m = r.method();
      if ((m === "POST" || m === "PUT") && r.url().indexOf("/api/playlists") >= 0) {
        publishRequests.push(m + " " + new URL(r.url()).pathname + new URL(r.url()).search);
      }
    });

    // Сценарии идут последовательно: состояние диска общее.
    for (const [name, fn] of scenarios) {
      if (serverDead) {
        console.error("[e2e-saveasnew] server dead, aborting");
        failed++;
        break;
      }
      try {
        await fn(page);
        console.log("  ✓ " + name);
      } catch (e) {
        failed++;
        console.error("  ✗ " + name);
        console.error("      " + String((e && e.message) || e).split("\n")[0]);
        try {
          const st = await pathState(page);
          console.error("      path: " + JSON.stringify(st));
        } catch {
          /* page недоступна */
        }
      }
    }
  } catch (e) {
    failed++;
    console.error("[e2e-saveasnew] fatal: " + String((e && e.message) || e));
  } finally {
    if (browser) await browser.close().catch(() => {});
    server.kill();
    try {
      rmSync(store, { recursive: true, force: true });
    } catch {
      /* ignore */
    }
  }

  console.log(
    failed === 0
      ? "[e2e-saveasnew] ALL PASSED (" + scenarios.length + ")"
      : "[e2e-saveasnew] FAILED: " + failed + " of " + scenarios.length,
  );
  process.exit(failed === 0 ? 0 : 1);
}

let baseUrl = null;

main();
