// Headless E2E для Muzlovar: публикация и переименование опубликованной
// подборки.
//
// Сценарии:
//   1. Новая подборка: первый Publish создаёт .nsp/.mix и state;
//   2. Смена названия (другой filename): блок «Опубликовано /
//      Будет опубликовано», Publish переезжает на новый файл,
//      старый удалён, чужие .nsp не тронуты, state переехал;
//   3. Перезагрузка страницы: показывается новый persisted-путь;
//   4. Смена названия без смены filename: блок не показывается,
//      Publish -> 409 -> #overwrite-dialog -> «Перезаписать»;
//   5. Файлы записаны мимо приложения: 422 cleanup_blocked,
//      ошибка в UI, файлы и state не тронуты, диалог не открывается.
//
// Запуск:  node publish-rename.mjs   (или npm run test:publish-rename)
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

// Точное содержимое блока с ожидающим переименованием.
async function waitPendingBlock(page, current, next) {
  const expected = "Опубликовано:\n" + current + "\n\nБудет опубликовано:\n" + next;
  await waitPathText(page, expected);
  return expected;
}

async function waitValidityOk(page) {
  await page.waitForFunction(
    () => {
      const t = document.getElementById("e-validity-text");
      const b = document.getElementById("e-validity");
      return b && b.className.indexOf("ok") >= 0 && t && t.textContent === "Проверено — можно публиковать";
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

// Ввести новое название и дождаться завершившейся проверки.
async function renameTo(page, name) {
  const v = nextValidate(page);
  await page.fill("#e-name", name);
  const res = await v;
  assert(res.ok(), "validate после переименования: HTTP " + res.status());
  // Ответ получен — runValidate дособирает UI в .then().
  await waitValidityOk(page);
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

function errorCodes(body) {
  if (Array.isArray(body.errors)) return body.errors.map((e) => e.code);
  if (body.error) return [body.error.code];
  return [];
}

function readState() {
  const p = path.join(rulesDir, ".muzlovar-published.json");
  if (!existsSync(p)) return null;
  return JSON.parse(readFileSync(p, "utf8"));
}

// ---------------------------------------------------------------- locations

let store = null;
let rulesDir = null;
let playlistsDir = null;

const nspOf = (slug) => path.join(playlistsDir, slug + ".nsp");
const mixOf = (slug) => path.join(rulesDir, slug + ".mix");

const bytes = (p) => readFileSync(p);

// ---------------------------------------------------------------- scenarios

async function scenario1_newPublish(page) {
  await page.goto(baseUrl + "/new", { waitUntil: "networkidle" });
  await page.waitForSelector('.ing[data-chip-field="год"]', { timeout: 15000 });

  // Название + одно условие — иначе пустое дерево не валидно.
  await page.fill("#e-name", "Publish E2E");
  await page.click('.ing[data-chip-field="год"]');
  await waitValidityOk(page);

  // До публикации блок показывает только каталог.
  let st = await pathState(page);
  assertEq(st.published, "0", "data-published до первой публикации");
  await waitPathText(page, st.dir);

  const w = nextPublish(page, "POST");
  await page.click("#e-publish");
  const res = await w;
  assertEq(res.status(), 201, "статус первой публикации");

  // Файлы и state созданы.
  assert(existsSync(nspOf("publish-e2e")), "нет publish-e2e.nsp");
  assert(existsSync(mixOf("publish-e2e")), "нет publish-e2e.mix");
  const state = readState();
  assert(state && state.playlists.length === 1, "state после первой публикации");
  assertEq(state.playlists[0].slug, "publish-e2e", "slug в state");

  // После публикации — только фактический путь, без «Будет опубликовано”.
  const next = joinLike(st.dir, "publish-e2e.nsp");
  await waitPathText(page, next);
  st = await pathState(page);
  assertEq(st.published, "1", "data-published после публикации");
  assertEq(st.slug, "publish-e2e", "data-slug после публикации");
  assertEq(st.current, next, "data-published-path после публикации");
  assert(page.url().indexOf("/edit/publish-e2e") >= 0, "URL не переехал: " + page.url());

  // Чужие файлы для последующих проверок: обычный посторонний .nsp
  // и пара, записанная мимо приложения (без записи в state).
  writeFileSync(nspOf("foreign"), bytes(nspOf("publish-e2e")));
  writeFileSync(nspOf("external"), bytes(nspOf("publish-e2e")));
  writeFileSync(mixOf("external"), bytes(mixOf("publish-e2e")));
}

async function scenario2_renamePublish(page) {
  const foreignBefore = bytes(nspOf("foreign"));
  const oldNsp = bytes(nspOf("publish-e2e"));
  const oldMix = bytes(mixOf("publish-e2e"));

  const before = await pathState(page);
  await renameTo(page, "Новое Название");

  // Блок ровно в требуемом формате.
  const next = joinLike(before.dir, "novoe-nazvanie.nsp");
  await waitPendingBlock(page, before.current, next);

  const w = nextPublish(page, "PUT");
  await page.click("#e-publish");
  const res = await w;
  assertEq(res.status(), 200, "статус переименования");

  // Новый файл есть, старого нет, чужие файлы не тронуты.
  assert(existsSync(nspOf("novoe-nazvanie")), "нет novoe-nazvanie.nsp");
  assert(existsSync(mixOf("novoe-nazvanie")), "нет novoe-nazvanie.mix");
  assert(!existsSync(nspOf("publish-e2e")), "publish-e2e.nsp остался");
  assert(!existsSync(mixOf("publish-e2e")), "publish-e2e.mix остался");
  assert(
    Buffer.compare(bytes(nspOf("foreign")), foreignBefore) === 0,
    "чужой foreign.nsp изменён",
  );
  assert(
    Buffer.compare(bytes(nspOf("external")), oldNsp) === 0 &&
      Buffer.compare(bytes(mixOf("external")), oldMix) === 0,
    "чужая пара external изменена",
  );

  // State переехал: только новый slug и новый путь.
  const state = readState();
  assertEq(state.playlists.length, 1, "число записей state");
  assertEq(state.playlists[0].slug, "novoe-nazvanie", "slug в state после rename");
  assert(
    state.playlists[0].nsp.path.indexOf("novoe-nazvanie.nsp") >= 0,
    "путь .nsp в state не новый: " + state.playlists[0].nsp.path,
  );

  // После успеха показывается только новый фактический путь.
  await waitPathText(page, next);
  const st = await pathState(page);
  assertEq(st.slug, "novoe-nazvanie", "data-slug после rename");
  assertEq(st.current, next, "data-published-path после rename");
  assert(
    page.url().indexOf("/edit/novoe-nazvanie") >= 0,
    "URL после rename: " + page.url(),
  );

  // Старый slug больше не находится.
  const gone = await fetch(baseUrl + "/api/playlists/publish-e2e");
  assertEq(gone.status, 404, "старый slug после rename");

  // Нет остаточных .tmp-файлов стадийного замещения.
  const leftovers = [...readdirSync(rulesDir), ...readdirSync(playlistsDir)].filter(
    (f) => f.indexOf(".tmp") >= 0,
  );
  assertEq(leftovers.length, 0, "остались временные файлы");
}

async function scenario3_reloadPersisted(page) {
  const { dir } = await pathState(page);
  const expected = joinLike(dir, "novoe-nazvanie.nsp");
  await page.goto(baseUrl + "/edit/novoe-nazvanie", { waitUntil: "networkidle" });
  await page.waitForSelector("#e-path", { timeout: 15000 });
  await waitValidityOk(page);

  const st = await pathState(page);
  assertEq(st.published, "1", "data-published после перезагрузки");
  assertEq(st.current, expected, "persisted путь после перезагрузки");
  await waitPathText(page, expected);
  assert(
    st.text.indexOf("Будет опубликовано") < 0,
    "блок переименования остался после перезагрузки: " + st.text,
  );
}

async function scenario4_sameFilenameOverwrite(page) {
  const before = await pathState(page);
  // Другое написание того же slug: filename не меняется.
  await renameTo(page, "Novoe Nazvanie");

  // Блок «Будет опубликовано” не показывается — путь только текущий.
  await waitPathText(page, before.current);
  const st = await pathState(page);
  assert(
    st.text.indexOf("Опубликовано") < 0,
    "блок переименования при неизменном filename: " + st.text,
  );

  // Целевой файл уже существует -> 409 -> диалог перезаписи.
  const conflict = nextPublish(page, "PUT");
  await page.click("#e-publish");
  const cRes = await conflict;
  assertEq(cRes.status(), 409, "статус без overwrite");
  await page.waitForFunction(
    () => {
      const d = document.getElementById("overwrite-dialog");
      return d && d.open;
    },
    null,
    { timeout: 5000 },
  );

  const w = nextPublish(page, "PUT", "overwrite=1");
  await page.click("#overwrite-dialog button.primary");
  const res = await w;
  assertEq(res.status(), 200, "статус с overwrite");
  await page.waitForFunction(
    () => !document.getElementById("overwrite-dialog").open,
    null,
    { timeout: 5000 },
  );

  // Файлы на месте, state не изменился, показывается текущий путь.
  assert(existsSync(nspOf("novoe-nazvanie")), "novoe-nazvanie.nsp пропал");
  assert(existsSync(mixOf("novoe-nazvanie")), "novoe-nazvanie.mix пропал");
  const state = readState();
  assertEq(state.playlists.length, 1, "число записей state после overwrite");
  assertEq(state.playlists[0].slug, "novoe-nazvanie", "slug в state после overwrite");
  await waitPathText(page, before.current);
}

async function scenario5_cleanupBlocked(page) {
  const extNsp = bytes(nspOf("external"));
  const extMix = bytes(mixOf("external"));
  const foreign = bytes(nspOf("foreign"));
  const stateBefore = JSON.stringify(readState());

  await page.goto(baseUrl + "/edit/external", { waitUntil: "networkidle" });
  await page.waitForSelector("#e-path", { timeout: 15000 });
  await waitValidityOk(page);

  await renameTo(page, "Blocked E2E");
  // Ожидающее переименование видно: пара не подтверждена state.
  const st = await pathState(page);
  await waitPendingBlock(page, st.current, joinLike(st.dir, "blocked-e2e.nsp"));

  const w = nextPublish(page, "PUT");
  await page.click("#e-publish");
  const res = await w;
  assertEq(res.status(), 422, "статус cleanup_blocked");
  const body = await res.json();
  assert(
    errorCodes(body).indexOf("cleanup_blocked") >= 0,
    "нет кода cleanup_blocked: " + JSON.stringify(body),
  );

  // UI-ошибка, диалог перезаписи НЕ открывается.
  await page.waitForFunction(
    () => {
      const e = document.getElementById("e-errors");
      return (
        e &&
        !e.hidden &&
        e.textContent.indexOf("Невозможно безопасно удалить ранее опубликованный файл") >= 0
      );
    },
    null,
    { timeout: 5000 },
  );
  const dlgOpen = await page.$eval("#overwrite-dialog", (d) => d.open);
  assert(!dlgOpen, "diалог перезаписи открылся на cleanup_blocked");

  // Ничего не изменилось: чужая пара, foreign, state, целевой файл.
  assert(
    Buffer.compare(bytes(nspOf("external")), extNsp) === 0 &&
      Buffer.compare(bytes(mixOf("external")), extMix) === 0,
    "чужая пара external изменена при cleanup_blocked",
  );
  assert(
    Buffer.compare(bytes(nspOf("foreign")), foreign) === 0,
    "foreign.nsp изменён при cleanup_blocked",
  );
  assertEq(JSON.stringify(readState()), stateBefore, "state изменился при cleanup_blocked");
  assert(!existsSync(nspOf("blocked-e2e")), "blocked-e2e.nsp создан");
  assert(!existsSync(mixOf("blocked-e2e")), "blocked-e2e.mix создан");
}

const scenarios = [
  ["1. первая публикация новой подборки", scenario1_newPublish],
  ["2. переименование: новый файл опубликован, старый удалён", scenario2_renamePublish],
  ["3. перезагрузка: persisted-путь нового файла", scenario3_reloadPersisted],
  ["4. filename не изменился: overwrite через диалог", scenario4_sameFilenameOverwrite],
  ["5. файлы без state: cleanup_blocked, всё не тронуто", scenario5_cleanupBlocked],
];

// ---------------------------------------------------------------- main

async function main() {
  // 1. build
  if (!process.env.MUZLOVAR_E2E_SKIP_BUILD) {
    console.log("[e2e-rename] cabal build exe:muzlovar ...");
    const r = spawnSync("cabal build exe:muzlovar", {
      cwd: repoRoot,
      stdio: "inherit",
      shell: true,
    });
    if (r.status !== 0) {
      console.error(
        "[e2e-rename] cabal build failed — если ошибка «Permission denied» на muzlovar.exe, " +
          "закройте запущенный сервер и повторите (или задайте MUZLOVAR_E2E_SKIP_BUILD=1).",
      );
      process.exit(1);
    }
  }
  const lb = process.env.MUZLOVAR_E2E_EXE ? { status: 0, stdout: process.env.MUZLOVAR_E2E_EXE } : spawnSync("cabal list-bin exe:muzlovar", {
    cwd: repoRoot,
    shell: true,
    encoding: "utf8",
  });
  const exePath = (lb.stdout || "").trim();
  if (lb.status !== 0 || !exePath || !existsSync(exePath)) {
    throw new Error("cabal list-bin exe:muzlovar failed: " + (lb.stdout || lb.stderr || ""));
  }

  // 2. temp store (seed-файлов нет: всё создаётся через UI)
  store = mkdtempSync(path.join(tmpdir(), "muzlovar-e2e-rename-"));
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
  console.log("[e2e-rename] exe:  " + exePath);
  console.log("[e2e-rename] store:" + store);
  console.log("[e2e-rename] url:  " + baseUrl);
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
    if (code !== 0 && code !== null) console.error("[e2e-rename] server exited with " + code);
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
    page.on("pageerror", (e) => console.error("[e2e-rename] pageerror: " + e.message));

    // Сценарии идут последовательно: состояние диска общее.
    for (const [name, fn] of scenarios) {
      if (serverDead) {
        console.error("[e2e-rename] server dead, aborting");
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
    console.error("[e2e-rename] fatal: " + String((e && e.message) || e));
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
      ? "[e2e-rename] ALL PASSED (" + scenarios.length + ")"
      : "[e2e-rename] FAILED: " + failed + " of " + scenarios.length,
  );
  process.exit(failed === 0 ? 0 : 1);
}

let baseUrl = null;

main();
