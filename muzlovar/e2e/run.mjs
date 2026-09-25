// Headless E2E для Muzlovar: drag & drop ингредиентов из палитры
// непосредственно в дерево правил (и перенос уже существующих условий),
// плюс layout-проверки: палитра ингредиентов, сетка таблицы подборок,
// footer, header/навигация и ограничения поля «год»
// (сценарии 12–20, свой url/viewport).
//
// Запуск:  node run.mjs      (или npm test)
// Переменные:
//   MUZLOVAR_E2E_SKIP_BUILD=1  — не запускать cabal build (exe уже собран)
//   MUZLOVAR_E2E_CHROME=<путь> — свой путь к chrome/chromium.exe
//
// Обвязка: собирает exe:muzlovar, поднимает сервер на свободном порту
// с временным стором (seeded .mix), прогоняет сценарии Playwright
// в headless Chromium (каждый сценарий — свежая страница и свежий
// seed, публикации нет, диск не меняется), останавливает сервер и
// чистит временный каталог.

import { spawn, spawnSync } from "node:child_process";
import {
  existsSync,
  mkdtempSync,
  mkdirSync,
  readdirSync,
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

// ---------------------------------------------------------------- seed mix

// Порядок корневых узлов важен для сценариев:
//   0: любимое   1: год < 2010   2: вложенная группа «любое»   3: год < 2020
// последний корневой узел — условие (сценарий «drop в конец»).
// Ограничения «год»: min=0, step=1, максимума нет (yearSpec в Fields.hs).
// Сидовые значения ≥ 1990, дефолт палитры «год = 0» лежит на нижней
// границе — валидация после drop не чистит предпросмотр .mix.
const seedMix = `подборка "e2e-dnd"
где все {
  любимое
  год < 2010
  любое {
    жанр содержит "rock"
    год < 1990
  }
  год < 2020
}
`;

// Дополнительные подборки для layout-сценариев списка: столько строк,
// чтобы контент гарантированно перерос низкое окно (иначе проверка
// «footer не перекрывает последние строки» ничего не ловит).
const seedLayoutCount = 6;

// ---------------------------------------------------------------- helpers

const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

function assert(cond, msg) {
  if (!cond) throw new Error(msg);
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
  const candidates = [
    path.join(base, "chromium-1234", "chrome-win64", "chrome.exe"),
    path.join(base, "chromium-1234", "chrome-win", "chrome.exe"),
    path.join(base, "chromium_headless_shell-1234", "chrome-win64", "headless_shell.exe"),
    path.join(base, "chromium_headless_shell-1234", "chrome-win", "headless_shell.exe"),
  ];
  for (const c of candidates) if (existsSync(c)) return c;
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

// ---------------------------------------------------------------- selectors

const sel = {
  root: "#e-tree .group-root",
  rootHead: "#e-tree .group-root > .group-head",
  rootList: "#e-tree .group-root > ul.tree",
  children: "#e-tree .group-root > ul.tree > li.node",
  nested: "#e-tree .group-nested",
  nestedHead: "#e-tree .group-nested > .group-head",
  nestedList: "#e-tree .group-nested > ul.tree",
  nestedChildren: "#e-tree .group-nested > ul.tree > li.node",
  preview: "#e-preview",
  validity: "#e-validity",
  rulesView: "#e-rules",
  ghost: ".drag-ghost",
  indicator: ".drop-indicator:not(.hidden)",
  dropzone: ".dropzone",
};

const ing = (field) => `.ing[data-chip-field="${field}"]`;

// Сырой (trim) текст предпросмотра: сравнение в waitFreshMix и проверки
// indexOf/regExp работают и по исходной разметке .mix.
function mixText(page) {
  return page.$eval(sel.preview, (e) => e.textContent.trim());
}

// Дождаться обновления предпросмотра .mix после очередного изменения.
async function waitFreshMix(page, prev) {
  await page.waitForFunction(
    (p) => {
      const el = document.getElementById("e-preview");
      return el && el.textContent.trim() !== p;
    },
    prev,
    { timeout: 10000 },
  );
  return mixText(page);
}

async function waitMixIncludes(page, marker) {
  await page.waitForFunction(
    (m) => {
      const el = document.getElementById("e-preview");
      return el && el.textContent.includes(m);
    },
    marker,
    { timeout: 10000 },
  );
  return mixText(page);
}

// Дождаться, пока предпросмотр .mix не станет удовлетворять регулярке
// (устойчиво к промежуточным обновлениям после нескольких drop подряд).
async function waitMixMatches(page, re) {
  await page.waitForFunction(
    (src) => {
      const el = document.getElementById("e-preview");
      return el && new RegExp(src).test(el.textContent);
    },
    re.source,
    { timeout: 10000 },
  );
  return mixText(page);
}

// ---------------------------------------------------------------- geometry

async function box(page, selector, idx = 0) {
  const loc = page.locator(selector).nth(idx);
  await loc.scrollIntoViewIfNeeded();
  const b = await loc.boundingBox();
  if (!b) throw new Error("element not visible: " + selector + "[" + idx + "]");
  return b;
}

const atTop = (b) => ({ x: b.x + b.width / 2, y: b.y + 5 });
const atBottom = (b) => ({ x: b.x + b.width / 2, y: b.y + b.height - 5 });
const atCenter = (b) => ({ x: b.x + b.width / 2, y: b.y + b.height / 2 });

// Центр элемента, вычисленный по требованию (после прокрутки палитры).
const atCenterPromise = async (page, selector, idx = 0) =>
  atCenter(await box(page, selector, idx));

// ---------------------------------------------------------------- drag

async function doDrag(page, start, point, midCheck) {
  await page.mouse.move(start.x, start.y);
  await page.mouse.down();
  await page.mouse.move(start.x + 10, start.y + 4, { steps: 3 }); // порог 6px
  let stage = "ghost";
  try {
    await page.waitForSelector(sel.ghost, { timeout: 5000 });
    stage = "move+indicator";
    await page.mouse.move(point.x, point.y, { steps: 14 });
    await page.waitForSelector(sel.indicator, { timeout: 5000 });
    stage = "midCheck+up";
    if (midCheck) await midCheck(page);
    await page.mouse.up();
    stage = "ghost-detached";
    await page.waitForSelector(sel.ghost, { state: "detached", timeout: 5000 });
  } catch (e) {
    await page.mouse.up().catch(() => {});
    let hit = null;
    try {
      hit = await page.evaluate(([x, y]) => {
        const el = document.elementFromPoint(x, y);
        const kids = document.querySelectorAll("#e-tree .group-root > ul.tree > li.node");
        const last = kids[kids.length - 1];
        const r = last ? last.getBoundingClientRect() : null;
        return {
          point: { x, y },
          hit: el
            ? { tag: el.tagName, cls: String(el.className).slice(0, 60) }
            : null,
          kids: kids.length,
          lastRect: r
            ? { top: r.top, bottom: r.bottom, left: r.left, right: r.right }
            : null,
          scrollY: window.scrollY,
          vh: window.innerHeight,
        };
      }, [point.x, point.y]);
    } catch {
      /* ignore */
    }
    throw new Error(
      "drag не завершился, этап «" + stage + "\" (" + JSON.stringify({ start, point, hit }) + "): " +
        String((e && e.message) || e).split("\n")[0],
    );
  }
}

// Drag карточки из палитры (data-chip-field) в точку дерева.
// point может быть функцией — тогда точка вычисляется ПОСЛЕ прокрутки
// карточки в видимую область (иначе координаты успевают устареть).
async function dragFromPalette(page, field, pointOrFn, midCheck) {
  const card = page.locator(ing(field));
  await card.scrollIntoViewIfNeeded();
  const b = await card.boundingBox();
  if (!b) throw new Error("palette card not visible: " + field);
  const point =
    typeof pointOrFn === "function" ? await pointOrFn() : pointOrFn;
  await doDrag(page, atCenter(b), point, midCheck);
}

// Drag существующего узла за его drag-handle (⠿) в точку дерева.
async function dragHandleTo(page, handleSelector, point, midCheck) {
  const h = page.locator(handleSelector).first();
  await h.scrollIntoViewIfNeeded();
  const b = await h.boundingBox();
  if (!b) throw new Error("handle not visible: " + handleSelector);
  await doDrag(page, atCenter(b), point, midCheck);
}

// ---------------------------------------------------------------- layout

// Геометрия таблицы подборок: сетка заголовка, строки с их ячейками и
// колонка «Действия» с кнопками. null, если таблицы на странице нет.
async function tableGrid(page) {
  return page.evaluate(() => {
    const table = document.querySelector("table.list");
    if (!table) return null;
    const rect = (e) => {
      const r = e.getBoundingClientRect();
      return { left: r.left, right: r.right, top: r.top, bottom: r.bottom, width: r.width, height: r.height };
    };
    const header = [...table.querySelectorAll("thead th")];
    const headRow = table.querySelector("thead tr");
    return {
      table: rect(table),
      headRow: rect(headRow),
      header: header.map(rect),
      headerDisplays: header.map((c) => getComputedStyle(c).display),
      rows: [...table.querySelectorAll("tbody tr")].map((tr) => ({
        box: rect(tr),
        cells: [...tr.children].map(rect),
        displays: [...tr.children].map((c) => getComputedStyle(c).display),
        buttons: [...tr.querySelectorAll(".actions-row > *")].map((b) => ({
          text: b.textContent.trim(),
          ...rect(b),
        })),
      })),
      doc: {
        scrollW: document.documentElement.scrollWidth,
        clientW: document.documentElement.clientWidth,
      },
    };
  });
}

// Строки, разделители и колонка «Действия» согласованы: все ячейки —
// table-cell, ширины колонок совпадают с заголовком, строки равны
// ширине таблицы, кнопки разведены, документ не едет по горизонтали.
function assertTableGrid(g, label) {
  assert(g, label + ": таблица не найдена");
  assert(g.rows.length > 0, label + ": нет строк");
  assert(
    g.headerDisplays.every((d) => d === "table-cell"),
    label + ": ячейка заголовка не table-cell: " + JSON.stringify(g.headerDisplays),
  );
  g.rows.forEach((row, i) => {
    assert(
      row.displays.every((d) => d === "table-cell"),
      label + ": строка " + i + " содержит не table-cell: " + JSON.stringify(row.displays),
    );
    assert(
      row.cells.length === g.header.length,
      label + ": в строке " + i + " " + row.cells.length + " ячеек, в заголовке " + g.header.length,
    );
    row.cells.forEach((c, j) => {
      const dw = Math.abs(c.width - g.header[j].width);
      assert(
        dw < 1,
        label + ": колонка " + j + " в строке " + i + " отличается от заголовка на " + dw.toFixed(2) + "px",
      );
    });
    // Все строки таблицы (включая заголовочную) одной ширины. Сравниваем
    // со строкой заголовка, а не с table.clientWidth: у border-collapse:
    // separate браузер считает в клиентскую ширину ещё и внутренние
    // разделители, из-за чего она расходится с геометрией строк на ~2px.
    const dHead = Math.abs(row.box.width - g.headRow.width);
    assert(
      dHead < 1,
      label + ": строка " + i + " не той ширины, что строка заголовка (Δ=" + dHead.toFixed(2) + "px)",
    );
    // Колонка «Действия» — внутри строки, без разрывов с ней.
    const act = row.cells[row.cells.length - 1];
    assert(
      Math.abs(act.right - row.box.right) <= 1 && Math.abs(act.top - row.box.top) <= 1,
      label + ": колонка «Действия» оторвалась от строки " + i + ": " +
        JSON.stringify({ act, row: row.box }),
    );
    for (let k = 0; k + 1 < row.buttons.length; k++) {
      const a = row.buttons[k];
      const b = row.buttons[k + 1];
      const gapX = b.left - a.right;
      const gapY = b.top - a.bottom;
      assert(
        gapX >= 4 || gapY >= 4,
        label + ": кнопки «" + a.text + "»/«" + b.text + "» слиплись (Δx=" +
          gapX.toFixed(1) + ", Δy=" + gapY.toFixed(1) + ")",
      );
    }
  });
  assert(
    g.doc.scrollW <= g.doc.clientW + 1,
    label + ": документ прокручивается по горизонтали (" + g.doc.scrollW + " > " + g.doc.clientW + ")",
  );
}

// Layout-сценарии: свой url/viewport, ожидания редактора из общего цикла
// не нужны (ready — селектор, до которого ждём). Проверяют палитру
// ингредиентов, сетку таблицы подборок и footer — без изменения данных
// и поведения кнопок.
const layoutScenarios = [
  {
    name: "12. палитра: колонка по высоте окна, список прокручивается",
    url: "/edit/e2e-dnd",
    ready: ".ing",
    fn: async (page) => {
      const info = await page.evaluate(async () => {
        const col = document.querySelector(".col-left");
        const body = document.querySelector("#e-palette.col-body");
        const head = document.querySelector(".col-left .col-head");
        const search = document.querySelector(".col-left .col-search");
        const header = document.querySelector("header.app");
        const rightBody = document.querySelector(".col-right .col-body");
        const colFoot = document.querySelector(".col-foot");
        const out = {
          vh: window.innerHeight,
          headerBottom: header.getBoundingClientRect().bottom,
          colTop: col.getBoundingClientRect().top,
          colBottom: col.getBoundingClientRect().bottom,
          headTop: head.getBoundingClientRect().top,
          searchTop: search.getBoundingClientRect().top,
          groups: document.querySelectorAll(".ing-group").length,
          cards: document.querySelectorAll(".ing").length,
          scrollable: body.scrollHeight > body.clientHeight,
          clipped: [],
          footer: !!document.querySelector("footer.app"),
          colFootOverlap: false,
          colFootBottom: null,
        };
        // Группа не должна обрезаться: её содержимое целиком в её высоте.
        document.querySelectorAll(".ing-group").forEach((g) => {
          if (g.scrollHeight > g.clientHeight + 1) {
            out.clipped.push(g.querySelector(".ing-group-name").textContent);
          }
        });
        if (colFoot && rightBody) {
          out.colFootOverlap =
            rightBody.getBoundingClientRect().bottom > colFoot.getBoundingClientRect().top + 1;
          out.colFootBottom = colFoot.getBoundingClientRect().bottom;
        }
        body.scrollTop = body.scrollHeight;
        await new Promise((r) => setTimeout(r, 150));
        const cards = document.querySelectorAll(".ing");
        const last = cards[cards.length - 1].getBoundingClientRect();
        const bb = body.getBoundingClientRect();
        out.lastVisible = last.bottom <= bb.bottom + 1 && last.top >= bb.top - 1;
        out.headStays = Math.abs(head.getBoundingClientRect().top - out.headTop) < 1;
        out.searchStays = Math.abs(search.getBoundingClientRect().top - out.searchTop) < 1;
        out.bodyBottom = bb.bottom;
        return out;
      });
      const fields = await page.evaluate(async () => {
        const res = await fetch("/api/schema");
        const data = await res.json();
        return data.fields.length;
      });
      assert(info.groups === 9, "групп ингредиентов не 9: " + info.groups);
      assert(
        info.cards === fields,
        "в палитре " + info.cards + " ингредиентов, в схеме " + fields,
      );
      assert(
        info.colTop >= info.headerBottom - 1,
        "колонка ингредиентов залезла на шапку: " + info.colTop + " < " + info.headerBottom,
      );
      assert(
        info.colBottom <= info.vh + 1,
        "колонка ингредиентов ниже окна: " + info.colBottom + " > " + info.vh,
      );
      assert(info.scrollable, "список ингредиентов не прокручивается");
      assert(info.clipped.length === 0, "обрезанные группы: " + info.clipped.join(", "));
      assert(info.lastVisible, "последний ингредиент недоступен после прокрутки списка");
      assert(
        info.headStays && info.searchStays,
        "заголовок или поиск уехали при прокрутке списка",
      );
      assert(
        info.bodyBottom <= info.colBottom + 1,
        "список вышел за нижнюю границу колонки: " + info.bodyBottom + " > " + info.colBottom,
      );
      assert(!info.footer, "в редакторе не должно быть footer");
      assert(!info.colFootOverlap, "нижняя панель колонки перекрывает её содержимое");
      if (info.colFootBottom !== null) {
        assert(
          info.colFootBottom <= info.vh + 1,
          "нижняя панель колонки ниже окна: " + info.colFootBottom,
        );
      }
    },
  },
  {
    name: "13. палитра: группы сворачиваются, поиск работает",
    url: "/edit/e2e-dnd",
    ready: ".ing",
    fn: async (page) => {
      // Сворачиваем ту группу, где лежит «Год»: она должна показывать
      // совпадения поиска, не теряя при этом состояние свёрнутости.
      const idx = await page.evaluate(() => {
        const groups = [...document.querySelectorAll(".ing-group")];
        const hit = groups.findIndex((g) =>
          [...g.querySelectorAll(".ing")].some((c) =>
            c.querySelector(".ing-title").textContent.toLowerCase().includes("год"),
          ),
        );
        return hit;
      });
      assert(idx >= 0, "нет группы с ингредиентом «Год»");
      const group = page.locator(".ing-group").nth(idx);
      const head = group.locator(".ing-group-head").first();
      const list = group.locator(".ing-list").first();

      await head.click();
      assert(!(await list.isVisible()), "свёрнутая группа продолжает показывать список");
      assert(
        (await head.getAttribute("aria-expanded")) === "false",
        "aria-expanded не false после сворачивания",
      );

      await page.fill("#e-search", "год");
      const matched = await page.$$eval(".ing", (cards) =>
        cards.filter((c) => !c.hidden).map((c) => c.querySelector(".ing-title").textContent.toLowerCase()),
      );
      assert(matched.length > 0, "поиск не нашёл ингредиентов");
      assert(
        matched.every((t) => t.includes("год")),
        "поиск показал лишние ингредиенты: " + JSON.stringify(matched),
      );
      const hiddenGroups = await page.$$eval(".ing-group", (gs) => gs.filter((g) => g.hidden).length);
      assert(hiddenGroups > 0, "группы без совпадений не скрыты");
      assert(
        await page.$eval("#e-palette", (b) => b.classList.contains("searching")),
        "во время поиска не выставлен класс searching",
      );
      assert(await list.isVisible(), "совпадения в свёрнутой группе не показаны");

      await page.fill("#e-search", "");
      assert(
        !(await page.$eval("#e-palette", (b) => b.classList.contains("searching"))),
        "класс searching остался после очистки поиска",
      );
      assert(!(await list.isVisible()), "свёрнутая группа развернулась после поиска");
      const visible = await page.$$eval(".ing", (cards) => cards.filter((c) => !c.hidden).length);
      const total = await page.$$eval(".ing", (cards) => cards.length);
      assert(visible === total, "после очистки поиска видно " + visible + " из " + total);
      const shownGroups = await page.$$eval(".ing-group", (gs) => gs.filter((g) => !g.hidden).length);
      assert(
        shownGroups === await page.$$eval(".ing-group", (gs) => gs.length),
        "после очистки поиска скрылись группы",
      );

      await head.click();
      assert(await list.isVisible(), "развёрнутая группа не показывает список");
      assert(
        (await head.getAttribute("aria-expanded")) === "true",
        "aria-expanded не true после разворачивания",
      );
    },
  },
];

// ---------------------------------------------------------------- DOM state

// Форма корневого списка: ['cond:поле:оператор' | 'group'] по дочерним узлам.
function rootShape(page) {
  return page.$$eval(sel.children, (els) =>
    els.map((li) => {
      const g = li.querySelector(":scope > .group");
      if (g) return "group";
      const f = li.querySelector(":scope > .cond select.field-sel");
      const o = li.querySelector(':scope > .cond select[aria-label="Оператор"]');
      return "cond:" + (f ? f.value : "?") + ":" + (o ? o.value : "?");
    }),
  );
}

function nestedShape(page) {
  return page.$$eval(sel.nestedChildren, (els) =>
    els.map((li) => {
      const f = li.querySelector(":scope > .cond select.field-sel");
      const o = li.querySelector(':scope > .cond select[aria-label="Оператор"]');
      return "cond:" + (f ? f.value : "?") + ":" + (o ? o.value : "?");
    }),
  );
}

function childIds(page, selector) {
  return page.$$eval(selector, (els) => els.map((e) => e.dataset.id));
}

// Листья дерева в DOM-порядке (= порядок модели): title/оператор/значение
// из controls условия + DSL-ключ в форме, в которой их пишет рендерер .mix.
function domLeaves(page) {
  return page.$$eval("#e-tree .cond", (conds) =>
    conds
      .filter((c) => c.querySelector("select.field-sel"))
      .map((c) => {
        const fsel = c.querySelector("select.field-sel");
        const osel = c.querySelector('select[aria-label="Оператор"]');
        const num = c.querySelector('input[aria-label="Числовое значение"]');
        const txt = c.querySelector('input[aria-label="Текстовое значение"]');
        const bool = c.querySelector('select[aria-label="Значение"]');
        let val = "";
        if (num) val = num.value;
        else if (txt) val = txt.value;
        else if (bool) val = bool.selectedOptions[0].textContent;
        const field = fsel.value;
        const opText = osel.selectedOptions[0].textContent;
        const op = osel.value;
        let key;
        if (op === "bare") key = field;
        else if (txt) key = field + " " + opText + ' "' + val + '"';
        else key = field + " " + opText + " " + val;
        return {
          title: fsel.selectedOptions[0].textContent,
          opText,
          val,
          key,
        };
      }),
  );
}

// Листья read-only дерева модели (#e-rules, рендер из модели).
function viewLeaves(page) {
  return page.$$eval("#e-rules .vcond", (rows) =>
    rows
      .map((r) => {
        const f = r.querySelector(".vfield");
        if (!f) return null;
        const o = r.querySelector(".vop");
        const v = r.querySelector(".vval");
        return {
          title: f.textContent.trim(),
          opText: o ? o.textContent.trim() : "",
          val: v ? v.textContent.trim() : "",
        };
      })
      .filter(Boolean),
  );
}

function groupKinds(page, selector) {
  return page.$$eval(selector, (els) =>
    els.map((g) => (g.classList.contains("kind-any") ? "any" : "all")),
  );
}

async function diag(page) {
  try {
    return await page.evaluate(() => ({
      validity: (document.getElementById("e-validity-text") || {}).textContent || "",
      errors: ((document.getElementById("e-errors") || {}).textContent || "").slice(0, 300),
      preview: ((document.getElementById("e-preview") || {}).textContent || "")
        .replace(/\s+/g, " ")
        .slice(0, 300),
    }));
  } catch {
    return null;
  }
}

// ---------------------------------------------------------------- scenarios

const scenarios = [
  {
    name: "1. drop в начало группы",
    fn: async (page) => {
      const prev = await mixText(page);
      await dragFromPalette(page, "год", await atCenter(await box(page, sel.rootHead)));
      const shape = await rootShape(page);
      assert(
        shape[0] === "cond:год:eq",
        "новое условие не в начале корня: " + JSON.stringify(shape),
      );
      assert(shape.length === 5, "узлов стало не 5: " + JSON.stringify(shape));
      const mix = await waitFreshMix(page, prev);
      assert(mix.indexOf("год = 0") > 0, "нет нового условия в .mix: " + mix);
      assert(
        mix.indexOf("год = 0") < mix.indexOf("любимое"),
        "в .mix не в начале: " + mix,
      );
    },
  },
  {
    name: "2. drop между двумя условиями",
    fn: async (page) => {
      const prev = await mixText(page);
      const idsBefore = await childIds(page, sel.children);
      const second = await box(page, sel.children, 1); // год < 2010
      await dragFromPalette(page, "год", atTop(second), async (p) => {
        // DOM не должен меняться до pointerup: ни прыжков, ни индексов.
        const during = await childIds(p, sel.children);
        assert(
          JSON.stringify(during) === JSON.stringify(idsBefore),
          "DOM изменился во время drag: " + JSON.stringify({ idsBefore, during }),
        );
        // Индикатор: тонкая линия у границы вставки (верх второго узла).
        const ind = await p.$eval(".drop-indicator", (e) => {
          const r = e.getBoundingClientRect();
          return { top: r.top, height: r.height };
        });
        assert(Math.abs(ind.height - 2) < 0.6, "индикатор не 2px: " + ind.height);
        assert(
          ind.top > second.y - 30 && ind.top <= second.y + 8,
          "индикатор не у границы вставки: top=" + ind.top + " expected~" + second.y,
        );
        const hov = await p.$$eval(".group.drop-hover", (els) => els.length);
        assert(hov === 1, "подсветка группы под курсором: " + hov);
      });
      const shape = await rootShape(page);
      assert(
        JSON.stringify(shape) ===
          JSON.stringify([
            "cond:любимое:bare",
            "cond:год:eq",
            "cond:год:lt",
            "group",
            "cond:год:lt",
          ]),
        "порядок корня нарушен: " + JSON.stringify(shape),
      );
      const mix = await waitFreshMix(page, prev);
      const i0 = mix.indexOf("год = 0");
      assert(
        i0 > mix.indexOf("любимое") && i0 < mix.indexOf("год < 2010"),
        "в .mix не между: " + mix,
      );
    },
  },
  {
    name: "3. drop в конец группы",
    fn: async (page) => {
      const prev = await mixText(page);
      const last = await box(page, sel.children, 3); // год < 2020 — последний узел
      await dragFromPalette(page, "год", atBottom(last));
      const shape = await rootShape(page);
      assert(shape.length === 5, "узлов стало не 5: " + JSON.stringify(shape));
      assert(
        shape[4] === "cond:год:eq",
        "новое условие не в конце корня: " + JSON.stringify(shape),
      );
      const mix = await waitFreshMix(page, prev);
      assert(
        mix.indexOf("год = 0") > mix.indexOf("год < 2020"),
        "в .mix не в конце: " + mix,
      );
      assert(
        mix.indexOf("год = 0") > mix.indexOf("год < 1990"),
        "в .mix не после вложенной группы: " + mix,
      );
    },
  },
  {
    name: "4. drop во вложенную группу",
    fn: async (page) => {
      const prev = await mixText(page);
      // Шапка вложенной группы попадает в неё же → вставка в её начало.
      await dragFromPalette(page, "год", await atCenter(await box(page, sel.nestedHead)));
      const shape = await nestedShape(page);
      assert(
        JSON.stringify(shape) ===
          JSON.stringify(["cond:год:eq", "cond:жанр:contains", "cond:год:lt"]),
        "вложенная группа не получила условие первой: " + JSON.stringify(shape),
      );
      const root = await rootShape(page);
      assert(
        root.length === 4 && root[2] === "group",
        "корневая группа поехала: " + JSON.stringify(root),
      );
      const mix = await waitFreshMix(page, prev);
      assert(
        mix.indexOf("год = 0") > mix.indexOf("любое {") &&
          mix.indexOf("год = 0") < mix.indexOf("жанр"),
        "в .mix условие не внутри вложенной группы: " + mix,
      );
    },
  },
  {
    name: "5. два одинаковых поля (Год)",
    fn: async (page) => {
      const prev = await mixText(page);
      // Первый drop перед «год < 2010», второй — сразу после него:
      // три поля «Год» подряд, каждое с независимым id.
      await dragFromPalette(page, "год", await atTop(await box(page, sel.children, 1)));
      await dragFromPalette(page, "год", await atBottom(await box(page, sel.children, 2)));
      const shape = await rootShape(page);
      assert(
        JSON.stringify(shape) ===
          JSON.stringify([
            "cond:любимое:bare",
            "cond:год:eq",
            "cond:год:lt",
            "cond:год:eq",
            "group",
            "cond:год:lt",
          ]),
        "три поля «Год» не встали подряд: " + JSON.stringify(shape),
      );
      const ids = await page.$$eval(sel.children, (els) =>
        els.slice(1, 4).map((e) => e.dataset.id),
      );
      assert(new Set(ids).size === 3, "одинаковые id у разных условий: " + ids);
      const mix = await waitMixMatches(page, /год = 0\s+год < 2010\s+год = 0/);
      assert(
        /год = 0\s+год < 2010\s+год = 0/.test(mix),
        "в .mix поля «Год» не соседствуют: " + mix,
      );
    },
  },
  {
    name: "6. перестановка существующего условия",
    fn: async (page) => {
      const prev = await mixText(page);
      const idsBefore = await childIds(page, sel.children);
      const movedId = idsBefore[3]; // год < 2020 — последний узел
      await dragHandleTo(
        page,
        sel.children + ":nth-child(4) .drag-handle",
        await atCenter(await box(page, sel.rootHead)),
      );
      const shape = await rootShape(page);
      assert(
        shape[0] === "cond:год:lt",
        "условие не переставлено в начало: " + JSON.stringify(shape),
      );
      const idsAfter = await childIds(page, sel.children);
      assert(idsAfter[0] === movedId, "id перенесённого узла изменился (копия?)");
      const uniq = new Set(idsAfter);
      assert(uniq.size === idsAfter.length, "дубликаты id после переноса: " + idsAfter);
      const mix = await waitFreshMix(page, prev);
      assert(
        mix.indexOf("год < 2020") < mix.indexOf("любимое"),
        "порядок в .mix не изменился: " + mix,
      );
    },
  },
  {
    name: "7. перенос между группами",
    fn: async (page) => {
      const prev = await mixText(page);
      const nestedIds = await childIds(page, sel.nestedChildren);
      const movedId = nestedIds[0]; // жанр содержит "rock"
      // Из вложенной группы — в корень, перед «год < 2020».
      await dragHandleTo(
        page,
        sel.nestedChildren + ":nth-child(1) .drag-handle",
        await atTop(await box(page, sel.children, 3)),
      );
      const root = await rootShape(page);
      assert(
        JSON.stringify(root) ===
          JSON.stringify([
            "cond:любимое:bare",
            "cond:год:lt",
            "group",
            "cond:жанр:contains",
            "cond:год:lt",
          ]),
        "корень не получил условие из группы: " + JSON.stringify(root),
      );
      const nested = await nestedShape(page);
      assert(
        JSON.stringify(nested) === JSON.stringify(["cond:год:lt"]),
        "во вложенной группе остались лишние условия: " + JSON.stringify(nested),
      );
      const idsAfter = await childIds(page, sel.nestedChildren);
      assert(
        !(await page.$$eval(
          sel.children,
          (els, movedId) =>
            els.some((e) => e.dataset.id === movedId && e.querySelector(":scope > .group")),
          movedId,
        )),
        "перенесённый узел остался группой",
      );
      assert(idsAfter.length === 1, "nested ids: " + idsAfter);
      const mix = await waitFreshMix(page, prev);
      assert(
        mix.indexOf("жанр") > mix.indexOf("год < 1990") &&
          mix.indexOf("жанр") < mix.indexOf("год < 2020"),
        "условие не оказалось в корне: " + mix,
      );
    },
  },
  {
    name: "8. несколько последовательных перетаскиваний",
    fn: async (page) => {
      const counts = [];
      // два drop в начало + один перед последним узлом, без перезагрузки;
      // точки назначения считаются после прокрутки карточки палитры.
      // В конец специально НЕ целимся: точка у нижнего края колонки
      // запускает autoScroll, контент уезжает из-под фиксированного
      // указателя (сценарий 3 отдельно проверяет drop в конец).
      await dragFromPalette(page, "год", () => atCenterPromise(page, sel.rootHead));
      counts.push((await rootShape(page)).length);
      await dragFromPalette(page, "год", () => atCenterPromise(page, sel.rootHead));
      counts.push((await rootShape(page)).length);
      await dragFromPalette(page, "год", async () =>
        atTop(await box(page, sel.children, 5))); // прежний последний узел
      counts.push((await rootShape(page)).length);
      assert(
        JSON.stringify(counts) === JSON.stringify([5, 6, 7]),
        "узлы не добавлялись последовательно: " + JSON.stringify(counts),
      );
      await page.waitForFunction(
        () => ((document.getElementById("e-preview").textContent || "").match(/год = 0/g) || []).length >= 3,
        null,
        { timeout: 10000 },
      );
      const ids = await childIds(page, sel.children);
      assert(new Set(ids).size === ids.length, "дубликаты id после 3 drag: " + ids);
      assert((await page.$$(sel.ghost)).length === 0, "остался призрак");
      assert((await page.$$eval("body.dragging", (e) => e.length)) === 0, "body.dragging остался");
    },
  },
  {
    name: "9. клик по палитре после DnD",
    fn: async (page) => {
      const c0 = (await rootShape(page)).length;
      await dragFromPalette(page, "год", await atCenter(await box(page, sel.rootHead)));
      assert((await rootShape(page)).length === c0 + 1, "DnD не добавил условие");
      await waitMixIncludes(page, "год = 0");
      // Клик после drag: suppressClick гасит только клик, которым закончился drag.
      await page.click(ing("оценка"));
      assert((await rootShape(page)).length === c0 + 2, "клик после DnD не сработал");
      await waitMixIncludes(page, "оценка = 0");
      await page.click(ing("обложка"));
      const shape = await rootShape(page);
      assert(shape.length === c0 + 3, "повторный клик не сработал: " + JSON.stringify(shape));
      assert(
        shape[shape.length - 1] === "cond:обложка:eq",
        "клик добавил не в конец выбранной группы: " + JSON.stringify(shape),
      );
      await waitMixIncludes(page, "обложка = нет");
    },
  },
  {
    name: "10. после drop нет лишних элементов",
    fn: async (page) => {
      const paletteBefore = (await page.$$(sel.ing ? ".ing" : ".ing")).length;
      await dragFromPalette(page, "год", await atCenter(await box(page, sel.rootHead)));
      await waitMixIncludes(page, "год = 0");
      assert((await page.$$(sel.ghost)).length === 0, "остался drag-ghost");
      assert((await page.$$(".drop-indicator")).length === 0, "остался индикатор");
      assert((await page.$$eval("body.dragging", (e) => e.length)) === 0, "body.dragging остался");
      assert((await page.$$(sel.dropzone)).length === 0, "dropzone вернулся");
      assert(
        (await page.$$eval(".sortable-ghost, .sortable-chosen, .sortable-drag", (e) => e.length)) === 0,
        "остались классы SortableJS",
      );
      const bodyText = await page.$eval("body", (b) => b.textContent);
      assert(
        !bodyText.includes("Перетащите ингредиент сюда"),
        "низовая drop-zone не удалена из разметки",
      );
      const ids = await page.$$eval("#e-tree [data-id]", (els) =>
        els.map((e) => e.getAttribute("data-id")).filter(Boolean),
      );
      assert(new Set(ids).size === ids.length, "дубликаты data-id в дереве: " + ids);
      const paletteAfter = (await page.$$(".ing")).length;
      assert(paletteAfter === paletteBefore, "палитра изменилась после drag");
    },
  },
  {
    name: "11. порядок в DOM == модели == .mix",
    fn: async (page) => {
      const prev = await mixText(page);
      await dragFromPalette(page, "год", await atCenter(await box(page, sel.rootHead)));
      let mix = await waitFreshMix(page, prev);
      const last = await box(page, sel.children, 4);
      await dragFromPalette(page, "оценка", atBottom(last));
      mix = await waitFreshMix(page, mix);
      // Кнопки «Проверить» нет: предпросмотр .mix обновляется только
      // ответом автоматической валидации — waitFreshMix выше как раз
      // дождался её. Успех — «ok» либо «warn»: предупреждения (избыточные
      // условия и т. п.) не блокируют валидацию и не влияют на порядок.
      await page.waitForFunction(
        () => {
          const c = (document.getElementById("e-validity") || {}).className || "";
          return c.includes("ok") || c.includes("warn");
        },
        null,
        { timeout: 10000 },
      );
      mix = await mixText(page);

      const dom = await domLeaves(page);
      const view = await viewLeaves(page);
      assert(dom.length === view.length, "листьев в DOM и модели разное число");
      for (let i = 0; i < dom.length; i++) {
        assert(
          dom[i].title === view[i].title &&
            dom[i].opText === view[i].opText &&
            dom[i].val === view[i].val,
          "расхождение DOM/модель на листе " +
            i +
            ": " +
            JSON.stringify({ dom: dom[i], view: view[i] }),
        );
      }
      const domKinds = await groupKinds(page, "#e-tree .group");
      const viewKinds = await groupKinds(page, "#e-rules .vgroup");
      assert(
        JSON.stringify(domKinds) === JSON.stringify(viewKinds),
        "порядок/логика групп в DOM и модели расходятся: " +
          JSON.stringify({ domKinds, viewKinds }),
      );
      // Каждый лист из DOM встречается в .mix в том же относительном порядке.
      let pos = -1;
      for (const lf of dom) {
        const i = mix.indexOf(lf.key, pos + 1);
        assert(
          i > pos,
          "нарушен порядок в .mix у «" +
            lf.key +
            "» (pos=" +
            pos +
            "): " +
            mix,
        );
        pos = i;
      }
      assert(mix.startsWith('подборка "e2e-dnd"'), ".mix без имени: " + mix);
    },
  },
];

// Таблица подборок и footer: сетка колонок, колонка «Действия» и то, что
// подвал остаётся обычным элементом потока под прокручиваемым main.
const pageLayoutScenarios = [
  {
    name: "14. список подборок: согласованная сетка таблицы",
    url: "/",
    ready: "table.list tbody tr",
    fn: async (page) => {
      const g = await tableGrid(page);
      assertTableGrid(g, "широкий экран");
      const info = await page.evaluate(() => {
        const foot = document.querySelector("footer.app");
        const rows = [...document.querySelectorAll("table.list tbody tr")];
        const last = rows[rows.length - 1].getBoundingClientRect();
        const f = foot.getBoundingClientRect();
        return {
          footPos: getComputedStyle(foot).position,
          lastBottom: last.bottom,
          footTop: f.top,
          footBottom: f.bottom,
          vh: window.innerHeight,
        };
      });
      assert(info.footPos === "static", "footer должен быть в обычном потоке: " + info.footPos);
      assert(
        info.lastBottom <= info.footTop + 1,
        "footer перекрывает последнюю строку: " + info.lastBottom + " > " + info.footTop,
      );
      assert(
        info.footBottom <= info.vh + 1,
        "footer ниже окна: " + info.footBottom + " > " + info.vh,
      );
    },
  },
  {
    name: "15. список подборок: узкий экран без наезда и обрезания",
    url: "/",
    ready: "table.list tbody tr",
    viewport: { width: 700, height: 800 },
    fn: async (page) => {
      const g = await tableGrid(page);
      assertTableGrid(g, "узкий экран");
      // Широкая таблица прокручивается внутри main, а не документом.
      const reach = await page.evaluate(async () => {
        const main = document.querySelector("main");
        main.scrollLeft = main.scrollWidth;
        await new Promise((r) => setTimeout(r, 150));
        const cell = document.querySelector("table.list tbody tr .actions");
        const r = cell.getBoundingClientRect();
        const f = document.querySelector("footer.app").getBoundingClientRect();
        return {
          left: r.left,
          right: r.right,
          vw: window.innerWidth,
          footRight: f.right,
          scrollLeft: main.scrollLeft,
        };
      });
      assert(
        reach.right <= reach.vw + 1 && reach.left >= -1,
        "колонка «Действия» недостижима на узком экране: " + JSON.stringify(reach),
      );
      assert(reach.footRight <= reach.vw + 1, "footer шире окна: " + reach.footRight);
    },
  },
  {
    name: "16. footer не перекрывает контент при низком окне",
    url: "/",
    ready: "table.list tbody tr",
    viewport: { width: 1400, height: 460 },
    fn: async (page) => {
      const info = await page.evaluate(async () => {
        const main = document.querySelector("main");
        const foot = document.querySelector("footer.app");
        const rows = [...document.querySelectorAll("table.list tbody tr")];
        const scrollable = main.scrollHeight > main.clientHeight;
        main.scrollTop = main.scrollHeight;
        await new Promise((r) => setTimeout(r, 150));
        const last = rows[rows.length - 1].getBoundingClientRect();
        const f = foot.getBoundingClientRect();
        const m = main.getBoundingClientRect();
        return {
          footPos: getComputedStyle(foot).position,
          vh: window.innerHeight,
          clientH: document.documentElement.clientHeight,
          docScrollH: document.documentElement.scrollHeight,
          scrollable,
          mainBottom: m.bottom,
          lastBottom: last.bottom,
          lastTop: last.top,
          lastVisible: last.bottom <= m.bottom + 1 && last.top >= m.top - 1,
          footTop: f.top,
          footBottom: f.bottom,
        };
      });
      assert(info.footPos === "static", "footer должен быть в обычном потоке: " + info.footPos);
      assert(info.scrollable, "контент не перерос низкое окно: проверка потеряла смысл");
      assert(info.lastVisible, "последняя строка обрезана областью main: " + JSON.stringify(info));
      assert(
        info.footTop >= info.lastBottom - 1,
        "footer перекрывает последнюю строку: " + info.lastBottom + " > " + info.footTop,
      );
      assert(
        info.mainBottom <= info.footTop + 1,
        "footer начинается выше конца main: " + info.mainBottom + " > " + info.footTop,
      );
      assert(
        info.footBottom <= info.vh + 1,
        "footer ниже окна: " + info.footBottom + " > " + info.vh,
      );
      assert(
        info.docScrollH <= info.clientH + 1,
        "документ прокручивается вместо main: " + info.docScrollH + " > " + info.clientH,
      );
    },
  },
];

// Header, навигация и ограничения поля «год»: активный раздел на
// index/edit/new/trash, отсутствие шестерёнки/«Настройки» и кнопки
// «Проверить», узкий экран без горизонтального скролла документа,
// min/step года в /api/schema и в input редактора, серверная
// валидация граничных значений (−1 и 0).
const headerScenarios = [
  {
    name: "17. header: активный раздел навигации на всех страницах",
    url: "/",
    ready: "table.list tbody tr",
    fn: async (page) => {
      const origin = new URL(page.url()).origin;
      const checkNav = async (expectedHref, label) => {
        await page.waitForSelector(".app-nav .app-nav-link", { timeout: 15000 });
        const nav = await page.$$eval(".app-nav .app-nav-link", (els) =>
          els.map((a) => ({
            href: a.getAttribute("href"),
            text: a.textContent.trim(),
            active: a.classList.contains("active"),
            current: a.getAttribute("aria-current"),
          })),
        );
        assert(nav.length === 3, label + ": пунктов навигации " + nav.length + ": " + JSON.stringify(nav));
        assert(
          JSON.stringify(nav.map((n) => n.href)) === JSON.stringify(["/", "/new", "/trash"]),
          label + ": ссылки nav не те: " + JSON.stringify(nav),
        );
        const act = nav.filter((n) => n.active);
        assert(act.length === 1, label + ": активных пунктов " + act.length + ": " + JSON.stringify(nav));
        assert(
          act[0].href === expectedHref,
          label + ": активен «" + act[0].text + "» (" + act[0].href + "), ожидался " + expectedHref,
        );
        assert(act[0].current === "page", label + ": у активного пункта нет aria-current=page");
        for (const n of nav) {
          if (n.active) continue;
          assert(!n.current, label + ": неактивный «" + n.text + "» имеет aria-current=" + n.current);
        }
        const chrome = await page.evaluate(() => ({
          settingsBtn: !!document.getElementById("settings-btn"),
          settingsDialog: !!document.getElementById("settings-dialog"),
          eSave: !!document.getElementById("e-save"),
          checkBtn: [...document.querySelectorAll("button")].some(
            (b) => b.textContent.trim() === "Проверить",
          ),
          settingsText: document.body.textContent.includes("Настройки"),
        }));
        assert(!chrome.settingsBtn, label + ": осталась шестерёнка #settings-btn");
        assert(!chrome.settingsDialog, label + ": остался диалог #settings-dialog");
        assert(!chrome.eSave, label + ": осталась кнопка #e-save");
        assert(!chrome.checkBtn, label + ": осталась кнопка «Проверить»");
        assert(!chrome.settingsText, label + ": текст «Настройки» остался в DOM");
      };

      await checkNav("/", "index");
      await page.goto(origin + "/edit/e2e-dnd", { waitUntil: "networkidle" });
      await page.waitForSelector(ing("год"), { timeout: 15000 });
      await checkNav("/", "edit");
      await page.goto(origin + "/new", { waitUntil: "networkidle" });
      await page.waitForSelector(ing("год"), { timeout: 15000 });
      await checkNav("/new", "new");
      await page.goto(origin + "/trash", { waitUntil: "networkidle" });
      await checkNav("/trash", "trash");

      // Бейдж доступности: опрашивает /health самого Muzlovar
      // (внутренних терминов и намёков на Navidrome в нём нет).
      await page.waitForFunction(
        () => (document.getElementById("conn-text") || {}).textContent === "Muzlovar доступен",
        null,
        { timeout: 10000 },
      );
      const title = await page.$eval("#conn-status", (e) => e.getAttribute("title") || "");
      assert(title.includes("Muzlovar") && title.includes("/health"), "заголовок статуса: " + title);
    },
  },
  {
    name: "18. header на узком экране (700px): без горизонтального скролла",
    url: "/",
    ready: "table.list tbody tr",
    viewport: { width: 700, height: 800 },
    fn: async (page) => {
      const origin = new URL(page.url()).origin;
      const check = async (label) => {
        const info = await page.evaluate(() => ({
          scrollW: document.documentElement.scrollWidth,
          clientW: document.documentElement.clientWidth,
          links: [...document.querySelectorAll(".app-nav .app-nav-link")].map((a) => {
            const r = a.getBoundingClientRect();
            return { text: a.textContent.trim(), left: r.left, right: r.right, width: r.width };
          }),
        }));
        assert(
          info.scrollW <= info.clientW + 1,
          label + ": документ прокручивается по горизонтали (" + info.scrollW + " > " + info.clientW + ")",
        );
        assert(info.links.length === 3, label + ": пунктов nav " + info.links.length);
        for (const l of info.links) {
          assert(l.width > 0, label + ": ссылка «" + l.text + "» нулевой ширины");
          assert(
            l.left >= -1 && l.right <= info.clientW + 1,
            label + ": ссылка «" + l.text + "» за пределами окна: " + JSON.stringify(l),
          );
        }
      };
      await check("index");
      await page.goto(origin + "/edit/e2e-dnd", { waitUntil: "networkidle" });
      await page.waitForSelector(ing("год"), { timeout: 15000 });
      await check("editor");
      await page.goto(origin + "/trash", { waitUntil: "networkidle" });
      await page.waitForSelector(".app-nav .app-nav-link", { timeout: 15000 });
      await check("trash");
    },
  },
  {
    name: "19. год: /api/schema и input получают min=0, step=1, без max",
    fn: async (page) => {
      const schema = await page.evaluate(async () => {
        const res = await fetch("/api/schema");
        const data = await res.json();
        const f = data.fields.find((x) => x.id === "год");
        return f ? { min: f.min, max: f.max, step: f.step } : null;
      });
      assert(schema, "в /api/schema нет поля «год»");
      assert(schema.min === 0, "schema.min года = " + JSON.stringify(schema.min));
      assert(schema.step === 1, "schema.step года = " + JSON.stringify(schema.step));
      assert(
        schema.max === null || schema.max === undefined,
        "schema.max года = " + JSON.stringify(schema.max),
      );
      const inputs = await page.$$eval(
        '#e-tree .cond input[aria-label="Числовое значение"]',
        (els) =>
          els.map((i) => ({
            min: i.getAttribute("min"),
            max: i.getAttribute("max"),
            step: i.getAttribute("step"),
          })),
      );
      assert(inputs.length > 0, "в дереве нет числовых инпутов");
      for (const i of inputs) {
        assert(i.min === "0", "input min=" + JSON.stringify(i.min));
        assert(i.step === "1", "input step=" + JSON.stringify(i.step));
        assert(i.max === null, "input max=" + JSON.stringify(i.max) + " (не должен задаваться)");
      }
    },
  },
  {
    name: "20. год: /api/validate отклоняет −1 (422) и принимает 0 (200)",
    url: "/",
    ready: "table.list tbody tr",
    fn: async (page) => {
      const probe = (value) =>
        page.evaluate(async (v) => {
          const dto = {
            name: "e2e-год",
            public: false,
            root: { kind: "all", items: [{ type: "cond", field: "год", op: "eq", value: v }] },
          };
          const res = await fetch("/api/validate", {
            method: "POST",
            headers: { "Content-Type": "application/json" },
            body: JSON.stringify(dto),
          });
          const body = await res.json().catch(() => ({}));
          return { status: res.status, body };
        }, value);
      const bad = await probe(-1);
      assert(
        bad.status === 422,
        "год=-1: статус " + bad.status + ", ожидался 422: " + JSON.stringify(bad.body),
      );
      const msg = JSON.stringify(bad.body);
      assert(
        msg.includes("год") && msg.includes("диапазона"),
        "год=-1: неожиданное сообщение: " + msg,
      );
      const ok = await probe(0);
      assert(
        ok.status === 200,
        "год=0: статус " + ok.status + ", ожидался 200: " + JSON.stringify(ok.body),
      );
      assert(ok.body && ok.body.ok === true, "год=0: ok !== true: " + JSON.stringify(ok.body));
    },
  },
];

// Все сценарии подряд: поведение редактора, затем layout-проверки.
const allScenarios = [...scenarios, ...layoutScenarios, ...pageLayoutScenarios, ...headerScenarios];

// ---------------------------------------------------------------- main

async function main() {
  // 1. build
  if (!process.env.MUZLOVAR_E2E_SKIP_BUILD) {
    console.log("[e2e] cabal build exe:muzlovar ...");
    const r = spawnSync("cabal build exe:muzlovar", {
      cwd: repoRoot,
      stdio: "inherit",
      shell: true,
    });
    if (r.status !== 0) {
      console.error(
        "[e2e] cabal build failed — если ошибка «Permission denied» на muzlovar.exe, " +
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

  // 2. temp store
  const store = mkdtempSync(path.join(tmpdir(), "muzlovar-e2e-"));
  mkdirSync(path.join(store, "rules"), { recursive: true });
  writeFileSync(path.join(store, "rules", "e2e-dnd.mix"), seedMix, "utf8");
  // Подборки для layout-сценариев списка: строк должно хватать, чтобы
  // контент гарантированно перерос низкое окно.
  for (let i = 1; i <= seedLayoutCount; i++) {
    writeFileSync(
      path.join(store, "rules", "layout-" + i + ".mix"),
      'подборка "layout-' + i + '"\nгде все {\n  любимое\n}\n',
      "utf8",
    );
  }

  // 3. free port + server
  const port = await new Promise((resolve, reject) => {
    const srv = net.createServer();
    srv.listen(0, "127.0.0.1", () => {
      const p = srv.address().port;
      srv.close(() => resolve(p));
    });
    srv.on("error", reject);
  });
  const base = "http://127.0.0.1:" + port;
  console.log("[e2e] exe:  " + exePath);
  console.log("[e2e] store:" + store);
  console.log("[e2e] url:  " + base);
  const server = spawn(exePath, [], {
    env: {
      ...process.env,
      MUZLOVAR_USERNAME: "",
      MUZLOVAR_PASSWORD: "",
      MUZLOVAR_HOST: "127.0.0.1",
      MUZLOVAR_PORT: String(port),
      MUZLOVAR_RULES_DIR: path.join(store, "rules"),
      MUZLOVAR_PLAYLISTS_DIR: path.join(store, "playlists"),
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
    if (code !== 0 && code !== null) console.error("[e2e] server exited with " + code);
  });

  let failed = 0;
  let browser;
  try {
    await waitForServer(base + "/health");

    const chrome = findChrome();
    if (!chrome) throw new Error("chrome not found; set MUZLOVAR_E2E_CHROME");
    browser = await chromium.launch({ executablePath: chrome, headless: true });

    for (const sc of allScenarios) {
      if (serverDead) {
        console.error("[e2e] server dead, aborting");
        failed++;
        break;
      }
      const context = await browser.newContext({
        viewport: sc.viewport || { width: 1400, height: 900 },
      });
      const page = await context.newPage();
      try {
        await page.goto(base + (sc.url || "/edit/e2e-dnd"), { waitUntil: "networkidle" });
        if (sc.ready) {
          // Layout-сценарии ждут только свой селектор: им не нужны
          // модель и предпросмотр редактора.
          await page.waitForSelector(sc.ready, { timeout: 15000 });
        } else {
          await page.waitForSelector(sel.children, { timeout: 15000 });
          await page.waitForSelector(ing("год"), { timeout: 15000 });
          // Модель загружена и первый автотест валидации отработал.
          await page.waitForFunction(
            () => {
              const p = document.getElementById("e-preview");
              return p && p.textContent.includes("год < 1990");
            },
            null,
            { timeout: 15000 },
          );
        }
        await sc.fn(page);
        console.log("  ✓ " + sc.name);
      } catch (e) {
        failed++;
        console.error("  ✗ " + sc.name);
        console.error("      " + String((e && e.message) || e).split("\n")[0]);
        const d = await diag(page);
        if (d) {
          if (d.validity) console.error("      validity: " + d.validity);
          if (d.errors) console.error("      errors:   " + d.errors);
          console.error("      preview:  " + d.preview);
        }
      } finally {
        await context.close();
      }
    }
  } catch (e) {
    failed++;
    console.error("[e2e] fatal: " + String((e && e.message) || e));
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
      ? "[e2e] ALL PASSED (" + allScenarios.length + ")"
      : "[e2e] FAILED: " + failed + " of " + allScenarios.length,
  );
  process.exit(failed === 0 ? 0 : 1);
}

main();
