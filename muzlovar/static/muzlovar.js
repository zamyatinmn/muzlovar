/* Muzlovar — визуальный редактор умных подборок Navidrome.
 *
 * Раскладка desktop: шапка (логотип, навигация, доступность сервера,
 * «Опубликовать») и три колонки на всю высоту — ингредиенты (~22%),
 * рецепт подборки (~50%) и предпросмотр (~28%).
 *
 * Два принципа:
 *  1. Списки полей, операторов и групп ингредиентов приходят только
 *     с /api/schema — здесь нет собственных копий этих списков.
 *  2. Компилятора на JS нет: дерево — это DTO, а валидация и рендер
 *     выполняются сервером (/api/validate, публикация).
 */
(function () {
  'use strict';

  var schema = null;
  var model = null;          // PlaylistDto
  var slug = null;           // null — новая подборка
  var pendingSlug = null;    // slug из последней успешной проверки:
                             // каким станет filename после публикации
  var editable = true;
  var selectedGroupId = null; // id группы-приёмника клика по палитре
  var validateTimer = null;
  var lastValidateOk = false;

  var els = {};

  document.addEventListener('DOMContentLoaded', function () {
    cacheEls();
    initChrome();
    if (els.editor) {
      initEditor();
    }
    initList();
    initTrash();
  });

  function cacheEls() {
    [
      'editor', 'e-name', 'e-desc', 'e-public', 'e-limit',
      'e-status', 'e-palette', 'e-tree', 'e-sort', 'e-errors', 'e-preview', 'e-preview-nsp',
      'e-preview-wrap', 'e-personal', 'e-publish', 'e-publish-new', 'e-delete',
      'e-mix', 'e-nsp', 'toasts',
      'e-search', 'e-rules', 'e-validity', 'e-validity-text',
      'e-path', 'e-copy-mix', 'e-copy-preview', 'e-copy-nsp',
      'pl-title', 'pl-meta', 'tab-rules', 'tab-mix',
      'conn-status', 'conn-text'
    ].forEach(function (id) { els[id] = document.getElementById(id); });
  }

  /* ------------------------------------------------------------------ */
  /* Мелкие помощники                                                    */
  /* ------------------------------------------------------------------ */

  function el(tag, attrs, children) {
    var n = document.createElement(tag);
    if (attrs) Object.keys(attrs).forEach(function (k) {
      if (k === 'class') n.className = attrs[k];
      else if (k === 'text') n.textContent = attrs[k];
      else if (k === 'html') n.innerHTML = attrs[k];
      else if (k.slice(0, 2) === 'on') n.addEventListener(k.slice(2), attrs[k]);
      else if (attrs[k] !== null && attrs[k] !== undefined) n.setAttribute(k, attrs[k]);
    });
    (children || []).forEach(function (c) {
      if (c === null || c === undefined) return;
      n.appendChild(typeof c === 'string' ? document.createTextNode(c) : c);
    });
    return n;
  }

  function clear(node) { while (node.firstChild) node.removeChild(node.firstChild); }

  function toast(kind, text) {
    if (!els.toasts) return;
    var box = el('div', { class: 'state ' + kind, role: 'status', text: text });
    els.toasts.appendChild(box);
    setTimeout(function () { if (box.parentNode) box.parentNode.removeChild(box); }, 6000);
  }

  function api(path, opts) {
    opts = opts || {};
    var headers = { 'Accept': 'application/json' };
    if (opts.body !== undefined) headers['Content-Type'] = 'application/json';
    return fetch(path, {
      method: opts.method || 'GET',
      headers: headers,
      body: opts.body === undefined ? undefined : JSON.stringify(opts.body)
    }).then(function (r) {
      return r.json().catch(function () { return {}; }).then(function (data) {
        return { ok: r.ok, status: r.status, data: data };
      });
    });
  }

  function errorsOf(data) {
    if (Array.isArray(data.errors) && data.errors.length) return data.errors;
    if (data.error) return [data.error];
    return [];
  }

  /* Ошибки и (опционально) предупреждения. Предупреждения кода
   * «warning» не блокируют публикацию: при отсутствии ошибок бокс
   * показывается в виде warn. */
  function renderErrors(target, errors, heading, warnings) {
    if (!target) return;
    warnings = warnings || [];
    clear(target);
    if (!errors.length && !warnings.length) { target.hidden = true; return; }
    target.hidden = false;
    target.className = errors.length ? 'state error' : 'state warn';
    if (errors.length) {
      target.appendChild(el('h3', { text: heading || 'Ошибки' }));
      target.appendChild(errorList(errors));
    }
    if (warnings.length) {
      target.appendChild(el('h3', { text: 'Предупреждения' }));
      target.appendChild(errorList(warnings));
    }
  }

  function errorList(errors) {
    var ul = el('ul', { class: 'error-list' });
    errors.forEach(function (e) {
      var li = el('li');
      li.appendChild(document.createTextNode(e.message || String(e)));
      var pos = [];
      if (e.path) pos.push(e.path);
      if (e.line) pos.push('строка ' + e.line + (e.column ? ', столбец ' + e.column : ''));
      if (pos.length) li.appendChild(el('span', { class: 'pos', text: ' — ' + pos.join(' · ') }));
      ul.appendChild(li);
    });
    return ul;
  }

  function fieldById(id) {
    for (var i = 0; i < schema.fields.length; i++) {
      if (schema.fields[i].id === id) return schema.fields[i];
    }
    return null;
  }
  function operatorById(id) {
    for (var i = 0; i < schema.operators.length; i++) {
      if (schema.operators[i].id === id) return schema.operators[i];
    }
    return null;
  }
  function isPersonal(id) { return schema.personalFields.indexOf(id) >= 0; }

  /* ------------------------------------------------------------------ */
  /* Шапка, вкладки, копирование                                         */
  /* ------------------------------------------------------------------ */

  function initChrome() {
    initConnStatus();
    initTabs();
    bindCopy('e-copy-mix', function () { return els['e-mix'] ? els['e-mix'].value : ''; });
    bindCopy('e-copy-preview', function () {
      return els['e-preview'] ? els['e-preview'].textContent : '';
    });
    bindCopy('e-copy-nsp', function () {
      return els['e-preview-nsp'] ? els['e-preview-nsp'].textContent : '';
    });
  }

  /* Доступность сервера: опрашивает публичный /health.
   *
   * Разбор: /health — это эндпоинт самого Muzlovar, он всегда
   * отвечает 200 и НЕ проверяет Navidrome/Subsonic (см. routes в
   * Server.hs). Бейдж поэтому называет сервер редактора: внутренний
   * термин «Прод» и намёк на Navidrome тут были бы неверны. */
  function initConnStatus() {
    var box = els['conn-status'];
    if (!box) return;

    function setConn(ok) {
      box.className = 'conn ' + (ok ? 'ok' : 'down');
      if (els['conn-text']) {
        els['conn-text'].textContent = ok ? 'Muzlovar доступен' : 'Muzlovar недоступен';
      }
      box.title = ok
        ? 'Сервер Muzlovar отвечает на /health — проверка каждые 30 с'
        : 'Сервер Muzlovar не отвечает — обновите страницу позже';
    }

    function ping() {
      fetch('/health', { headers: { 'Accept': 'application/json' } })
        .then(function (r) { setConn(r.ok); })
        .catch(function () { setConn(false); });
    }

    ping();
    setInterval(ping, 30000);
  }

  function initTabs() {
    document.querySelectorAll('.tab[data-tab]').forEach(function (btn) {
      btn.addEventListener('click', function () { switchTab(btn.getAttribute('data-tab')); });
    });
  }

  function switchTab(key) {
    document.querySelectorAll('.tab[data-tab]').forEach(function (btn) {
      var on = btn.getAttribute('data-tab') === key;
      btn.classList.toggle('active', on);
      btn.setAttribute('aria-selected', on ? 'true' : 'false');
    });
    ['rules', 'mix', 'nsp'].forEach(function (k) {
      var panel = document.getElementById('tab-' + k);
      if (panel) panel.hidden = (k !== key);
    });
  }

  function bindCopy(btnId, getText) {
    var btn = els[btnId] || document.getElementById(btnId);
    if (!btn) return;
    var original = btn.textContent;
    btn.addEventListener('click', function () {
      var text = getText() || '';
      if (!text) { toast('warn', 'Нечего копировать.'); return; }
      var done = function () {
        btn.textContent = 'Скопировано ✓';
        setTimeout(function () { btn.textContent = original; }, 1500);
      };
      if (navigator.clipboard && navigator.clipboard.writeText) {
        navigator.clipboard.writeText(text).then(done, function () {
          legacyCopy(text); done();
        });
      } else {
        legacyCopy(text); done();
      }
    });
  }

  function legacyCopy(text) {
    var ta = document.createElement('textarea');
    ta.value = text;
    ta.style.position = 'fixed';
    ta.style.opacity = '0';
    document.body.appendChild(ta);
    ta.select();
    try { document.execCommand('copy'); } catch (e) { /* ignore */ }
    document.body.removeChild(ta);
  }

  /* ------------------------------------------------------------------ */
  /* Пути DTO                                                            */
  /* ------------------------------------------------------------------ */

  /* Индексный путь нужен только кнопкам удаления (свежий рендер →
   * путь актуален в момент клика). Перенос и приём клика работают по
   * стабильным id (findGroupById/findItem) — см. раздел Drag & drop. */

  function removeItemAt(itemPath) {
    var parts = itemPath.split('/').slice(1); // root, items, i, [items, j...]
    var g = model.root;
    var i = 1;
    while (i < parts.length - 2) {
      g = g.items[parseInt(parts[i + 1], 10)];
      if (!g) return null;
      i += 2;
    }
    var idx = parseInt(parts[parts.length - 1], 10);
    return g.items.splice(idx, 1)[0];
  }

  /* Группа по стабильному id: во время drag индексные пути устаревают
   * в момент переноса (снятие элемента меняет индексы всех соседей),
   * поэтому цель переноса всегда определяется по id группы. */
  function findGroupById(id) {
    if (!id || !model || !model.root) return null;
    if (model.root.id === id) return model.root;
    var walk = function (items) {
      for (var i = 0; i < items.length; i++) {
        var it = items[i];
        if (!it || it.type !== 'group') continue;
        if (it.id === id) return it;
        var r = walk(it.items || []);
        if (r) return r;
      }
      return null;
    };
    return walk(model.root.items);
  }

  /* Вставить элемент в группу: в позицию «перед элементом beforeId»
   * (null — в конец). Позиция задаётся id соседа, а не индексом. */
  function insertIntoGroup(groupId, beforeId, item) {
    var g = findGroupById(groupId);
    if (!g) return false;
    var idx = g.items.length;
    if (beforeId) {
      for (var i = 0; i < g.items.length; i++) {
        if (g.items[i] && g.items[i].id === beforeId) { idx = i; break; }
      }
    }
    g.items.splice(idx, 0, item);
    return true;
  }

  /* Перетаскивается ли группа внутрь самой себя (или своего
   * потомка): такой drop запрещён — иначе дерево разрывается. */
  function groupIsInsideItem(itemId, groupId) {
    var r = findItem(itemId);
    if (!r || !r.item || r.item.type !== 'group') return false;
    var walk = function (g) {
      if (g.id === groupId) return true;
      var items = g.items || [];
      for (var i = 0; i < items.length; i++) {
        if (items[i] && items[i].type === 'group' && walk(items[i])) return true;
      }
      return false;
    };
    return walk(r.item);
  }

  /* ------------------------------------------------------------------ */
  /* Стабильные идентификаторы элементов                                 */
  /* ------------------------------------------------------------------ */

  /* Поле («год», «жанр») не может быть ключом состояния: одинаковые
   * поля живут как независимые элементы со своими id. id служебный,
   * в DTO не сериализуется. */
  var idSeq = 0;

  function newId() {
    idSeq += 1;
    return 'it' + idSeq + '-' + Date.now().toString(36);
  }

  /* id получают все узлы дерева, включая корневую группу: id — это
   * единственная стабильная идентичность и элементов, и групп. */
  function ensureIds(dto) {
    if (dto && dto.root && !dto.root.id) dto.root.id = newId();
    var walk = function (items) {
      (items || []).forEach(function (it) {
        if (!it.id) it.id = newId();
        if (it.type === 'group') walk(it.items);
      });
    };
    walk(dto && dto.root ? dto.root.items : []);
  }

  function findItem(id) {
    var walk = function (items, path) {
      for (var i = 0; i < items.length; i++) {
        var it = items[i];
        var p = path + '/items/' + i;
        if (it.id === id) return { item: it, path: p, list: items, index: i };
        if (it.type === 'group') {
          var r = walk(it.items, p);
          if (r) return r;
        }
      }
      return null;
    };
    return walk(model.root.items, '/root');
  }

  function removeItemById(id) {
    var r = findItem(id);
    if (!r) return null;
    r.list.splice(r.index, 1);
    return r.item;
  }

  function condPath(groupPath, index) {
    return groupPath + '/items/' + index;
  }

  /* Выбранная для клика группа живёт по id: индексный путь перестаёт
   * указывать на ту же группу после любого переноса соседей. */
  function recomputeSelected() {
    if (!model) return;
    ensureIds(model);
    if (!findGroupById(selectedGroupId)) selectedGroupId = model.root.id;
  }

  /* ------------------------------------------------------------------ */
  /* Новая подборка                                                      */
  /* ------------------------------------------------------------------ */

  function emptyModel() {
    return {
      name: '',
      description: '',
      public: false,
      root: { kind: 'all', items: [] },
      sort: { kind: 'random' },
      limit: null
    };
  }

  function defaultCond(fieldId) {
    var f = fieldById(fieldId) || schema.fields[0];
    var ids = opIds(f);
    var opId = ids[0];
    return { id: newId(), type: 'cond', field: f.id, op: opId, value: defaultValue(f, opId) };
  }

  /* Операторы поля: /api/schema отдаёт объекты {id, valueType};
   * строки поддерживаются для совместимости со старыми ответами. */
  function opIds(f) {
    return (f.operators || []).map(function (o) {
      return typeof o === 'string' ? o : o.id;
    });
  }

  /* Категория операнда оператора (valueType в описании оператора
   * поля): 'none' | 'text' | 'number' | 'bool' | 'date' | 'days' |
   * 'numberRange' | 'dateRange' | 'playlistRef'. По ней выбирается
   * редактор значения; если схема не знает оператора — выводится по
   * полю. */
  function opValueType(f, opId) {
    var ops = (f && f.operators) || [];
    for (var i = 0; i < ops.length; i++) {
      if (typeof ops[i] !== 'string' && ops[i].id === opId) return ops[i].valueType;
    }
    return inferValueType(f, opId);
  }

  function inferValueType(f, opId) {
    if (opId === 'bare' || opId === 'isMissing' || opId === 'isPresent') return 'none';
    if (opId === 'inTheLast' || opId === 'notInTheLast') return 'days';
    if (opId === 'before' || opId === 'after') return 'date';
    if (opId === 'between') return (f && f.valueType === 'date') ? 'dateRange' : 'numberRange';
    if (opId === 'inPlaylist' || opId === 'notInPlaylist') return 'playlistRef';
    switch (f && f.valueType) {
      case 'bool': return 'bool';
      case 'number': return 'number';
      case 'date': return 'date';
      case 'playlistRef': return 'playlistRef';
      default: return 'text';
    }
  }

  /* Абсолютная дата в форме значения: ГГГГ-ММ-ДД. */
  function isDay(v) {
    return typeof v === 'string' && /^\d{4}-\d{2}-\d{2}$/.test(v);
  }

  /* Сегодняшняя дата в локальной таймзоне: ГГГГ-ММ-ДД с ведущими
   * нулями — та же форма, что принимают ядро и Navidrome. */
  function todayIso() {
    var d = new Date();
    var pad = function (n) { return (n < 10 ? '0' : '') + n; };
    return d.getFullYear() + '-' + pad(d.getMonth() + 1) + '-' + pad(d.getDate());
  }

  /* Поле даты: input[type=date] со значением ГГГГ-ММ-ДД. */
  function dateInput(value, setter, aria) {
    return el('input', {
      type: 'date',
      value: isDay(value) ? value : todayIso(),
      style: 'width:14ch',
      'aria-label': aria || 'Значение',
      disabled: editable ? null : 'disabled',
      oninput: function (e) { setter(e.target.value); scheduleValidate(); }
    });
  }

  /* Ограничения числового поля из схемы (min/max/step): те же
   * значения проверяет валидатор ядра, поэтому UI просто не даёт
   * выйти за них. Ключи попадают в атрибуты input[type=number].
   * Для дробных полей (integral=false) шаг по умолчанию «any»:
   * целочисленный step схемы для них не задаётся. */
  function numAttrs(field) {
    var a = {};
    if (field.min !== null && field.min !== undefined) a.min = String(field.min);
    if (field.max !== null && field.max !== undefined) a.max = String(field.max);
    if (field.step !== null && field.step !== undefined) a.step = String(field.step);
    else if (field.integral === false) a.step = 'any';
    return a;
  }

  function addNumAttrs(attrs, field) {
    var a = numAttrs(field);
    Object.keys(a).forEach(function (k) { attrs[k] = a[k]; });
    return attrs;
  }

  /* Приводит число к допустимому диапазону поля (само поле не
   * переписывается — это делает ввод, чтобы не ломать набор текста). */
  function clampNum(field, v) {
    if (typeof v !== 'number' || isNaN(v)) v = 0;
    if (field.min !== null && field.min !== undefined && v < field.min) v = field.min;
    if (field.max !== null && field.max !== undefined && v > field.max) v = field.max;
    return v;
  }

  function defaultValue(field, op) {
    switch (opValueType(field, op)) {
      case 'none':
        return null;
      case 'numberRange': {
        var lo = clampNum(field, 0);
        var hi = clampNum(field, 100);
        if (lo > hi) hi = lo;
        return [lo, hi];
      }
      case 'dateRange':
        return [todayIso(), todayIso()];
      case 'days':
        return 30;
      case 'playlistRef': {
        var refKinds = (field && field.refKinds) || [];
        return { kind: refKinds.length ? refKinds[0].id : '', value: '' };
      }
      case 'date':
        return todayIso();
      case 'bool':
        return false;
      case 'number':
        return clampNum(field, 0);
      default:
        if (field.enum && field.enum.length) return field.enum[0].value;
        return '';
    }
  }

  function needsValue(op) {
    return op !== 'bare' && op !== 'isMissing' && op !== 'isPresent';
  }

  /* Текстовое значение условия — для дерева предпросмотра. */
  function condValueText(field, item) {
    if (!needsValue(item.op)) return '';
    var vt = opValueType(field, item.op);
    if (vt === 'numberRange' || vt === 'dateRange') {
      var v = item.value;
      return (Array.isArray(v) && v.length === 2) ? (v[0] + '–' + v[1]) : '';
    }
    if (vt === 'days') {
      return String(item.value) + ' дн.';
    }
    if (vt === 'playlistRef') {
      var rv = item.value;
      if (!rv || typeof rv !== 'object') return '';
      var kindLabel = rv.kind;
      var refKinds = (field && field.refKinds) || [];
      for (var r = 0; r < refKinds.length; r++) {
        if (refKinds[r].id === rv.kind) kindLabel = refKinds[r].label;
      }
      return (kindLabel || rv.kind || '') + ': ' + (rv.value || '');
    }
    if (field && field.valueType === 'bool') {
      var variants = field.valueVariants || ['да', 'нет'];
      return item.value ? (variants[0] || 'да') : (variants[1] || 'нет');
    }
    if (field && field.enum && field.enum.length) {
      for (var i = 0; i < field.enum.length; i++) {
        if (field.enum[i].value === item.value) return field.enum[i].label;
      }
    }
    if (item.value === null || item.value === undefined || item.value === '') return '';
    return String(item.value);
  }

  /* ------------------------------------------------------------------ */
  /* Инициализация редактора                                             */
  /* ------------------------------------------------------------------ */

  function initEditor() {
    var raw = els.editor.getAttribute('data-slug');
    slug = raw && raw.length ? raw : null;
    model = emptyModel();
    initDrag();

    renderStatus('Загрузка…', 'loading');

    api('/api/schema').then(function (res) {
      if (!res.ok) throw new Error('Не удалось загрузить схему полей.');
      schema = res.data;
      return slug ? api('/api/playlists/' + encodeURIComponent(slug)) : null;
    }).then(function (res) {
      if (res) {
        if (!res.ok) {
          renderStatus(errorText(res), 'error');
          setActions(false);
          return null;
        }
        var d = res.data;
        if (d.playlist) model = normalize(d.playlist);
        editable = d.editable !== false;
        els['e-mix'].value = d.mix || '';
        els['e-nsp'].value = d.nsp || '';
        if (d.external) renderStatus('Внешняя подборка: дерево содержит конструкции, неизвестные редактору. Редактирование отключено.', 'warn');
        else renderStatus('', null);
      } else {
        renderStatus('', null);
      }
      if (!editable) setActions(false);
      buildPalette();
      bindPaletteClicks();
      bindSearch();
      bindForm();
      renderAll();
      updatePath();
      scheduleValidate(0);
      return null;
    }).catch(function (e) {
      renderStatus(String(e.message || e), 'error');
      setActions(false);
    });
  }

  function errorText(res) {
    var errs = errorsOf(res.data);
    if (errs.length) return errs.map(function (e) { return e.message; }).join(' ');
    return 'Ошибка HTTP ' + res.status;
  }

  function normalize(dto) {
    if (!dto.root) dto.root = { kind: 'all', items: [] };
    if (!dto.root.items) dto.root.items = [];
    if (dto.description === undefined || dto.description === null) dto.description = '';
    if (dto.limit === undefined) dto.limit = null;
    if (!dto.sort) dto.sort = { kind: 'random' };
    ensureIds(dto);
    return dto;
  }

  function renderStatus(text, kind) {
    if (!els['e-status']) return;
    if (!text) { els['e-status'].hidden = true; clear(els['e-status']); return; }
    els['e-status'].hidden = false;
    els['e-status'].className = 'state ' + (kind || 'loading');
    els['e-status'].textContent = text;
  }

  function setActions(on) {
    ['e-publish', 'e-publish-new', 'e-delete'].forEach(function (id) {
      if (els[id]) els[id].disabled = !on;
    });
    var tree = els['e-tree'];
    if (tree) tree.querySelectorAll('button, select, input').forEach(function (n) {
      n.disabled = !on;
    });
  }

  /* ------------------------------------------------------------------ */
  /* Форма метаданных                                                    */
  /* ------------------------------------------------------------------ */

  function bindForm() {
    els['e-name'].addEventListener('input', function () {
      model.name = els['e-name'].value;
      updatePreviewCard();
      scheduleValidate();
    });
    els['e-desc'].addEventListener('input', function () {
      model.description = els['e-desc'].value; scheduleValidate();
    });
    els['e-public'].addEventListener('change', function () {
      model.public = els['e-public'].checked; scheduleValidate();
    });
    els['e-limit'].addEventListener('input', function () {
      var v = els['e-limit'].value.trim();
      if (v === '') model.limit = null;
      else {
        var n = parseInt(v, 10);
        model.limit = isNaN(n) ? -1 : n;
      }
      updatePreviewCard();
      scheduleValidate();
    });

    // Персональные поля — предупреждение в UI.
    if (els['e-personal'] && schema.personalFields.length) {
      els['e-personal'].textContent =
        'Персональные поля (' + schema.personalFields.join(', ') + ') зависят от пользователя ' +
        'и времени прослушивания — одно правило даёт разный плейлист.';
      els['e-personal'].hidden = false;
    }

    if (els['e-publish']) els['e-publish'].addEventListener('click', publish);
    if (els['e-publish-new']) els['e-publish-new'].addEventListener('click', publishAsNew);
    if (els['e-delete']) els['e-delete'].addEventListener('click', confirmDelete);
    syncPublishButtons();
  }

  /* Подпись основной кнопки публикации и видимость «Сохранить как
   * новую»: у неопубликованной подборки только «Опубликовать», у
   * опубликованной — «Сохранить изменения» плюс сохранение копией. */
  function syncPublishButtons() {
    if (els['e-publish']) {
      els['e-publish'].textContent = isPublished() ? 'Сохранить изменения' : 'Опубликовать';
    }
    if (els['e-publish-new']) els['e-publish-new'].hidden = !isPublished();
  }

  /* Путь публикации — только информация (кнопки и редактирования нет).
   * До первой публикации показывается настроенный каталог .nsp из
   * конфигурации сервера (data-publish-dir), после — фактический путь
   * опубликованного файла (data-published-path). Состояние
   * публикации — data-published, см. updatePath. */
  function publishDir() {
    if (!els.editor) return '';
    return els.editor.getAttribute('data-publish-dir') || '';
  }

  function isPublished() {
    return !!els.editor && els.editor.getAttribute('data-published') === '1';
  }

  /* Фактический путь опубликованного .nsp (server-rendered, после
   * публикации обновляется из ответа). */
  function publishedPath() {
    if (!els.editor) return '';
    return els.editor.getAttribute('data-published-path') || '';
  }

  /* Склейка каталога и имени файла с разделителем, принятым в каталоге
   * (на Windows путь конфигурации может быть обратными слэшами). */
  function publishFilePath(name) {
    var dir = publishDir();
    if (!dir) return name;
    var trimmed = dir.replace(/[\\\/]+$/, '');
    if (!trimmed) return name;
    var sep = trimmed.indexOf('\\') >= 0 && trimmed.indexOf('/') < 0 ? '\\' : '/';
    return trimmed + sep + name;
  }

  /* Блок «Путь публикации»:
   *  - не опубликована — только каталог;
   *  - опубликована и filename не меняется — фактический путь;
   *  - опубликована и новое название даёт другой filename — вторая
   *    половина «Будет опубликовано» до следующей публикации. */
  function updatePath() {
    var p = els['e-path'];
    if (!p) return;
    var current = publishedPath();
    if (!isPublished() || !current) {
      p.textContent = publishDir() || '—';
      return;
    }
    var next = pendingSlug ? pendingSlug + '.nsp' : '';
    var curName = current.split(/[\\\/]/).pop();
    if (next && next !== curName) {
      p.textContent =
        'Опубликовано:\n' + current +
        '\n\nБудет опубликовано:\n' + publishFilePath(next);
    } else {
      p.textContent = current;
    }
  }

  /* Карточка подборки: название и число треков (по лимиту). */
  function updatePreviewCard() {
    var title = els['pl-title'];
    var meta = els['pl-meta'];
    if (!title || !meta || !model) return;
    var name = (model.name || '').trim();
    title.textContent = name || 'Без названия';
    var limit = model.limit;
    if (limit === null || limit === undefined || limit < 0) {
      meta.textContent = 'Лимит не задан';
    } else {
      meta.textContent = limit + ' ' + trackWord(limit);
    }
  }

  function trackWord(n) {
    var n10 = n % 10;
    var n100 = n % 100;
    if (n10 === 1 && n100 !== 11) return 'трек';
    if (n10 >= 2 && n10 <= 4 && (n100 < 12 || n100 > 14)) return 'трека';
    return 'треков';
  }

  /* ------------------------------------------------------------------ */
  /* Ингредиенты (палитра)                                               */
  /* ------------------------------------------------------------------ */

  function typeGlyph(valueType) {
    switch (valueType) {
      case 'bool': return '✓';
      case 'number': return '#';
      case 'date': return '◷';
      case 'playlistRef': return '⇄';
      default: return 'A';
    }
  }

  function buildPalette() {
    var box = els['e-palette'];
    if (!box) return;
    clear(box);

    // Группы и состав приходят со схемы: копий списков полей здесь нет.
    var defs = (schema.ingredientGroups && schema.ingredientGroups.length)
      ? schema.ingredientGroups
      : [{ id: 'all', name: 'Ингредиенты' }];
    var order = [];
    var byId = {};

    function bucket(id, name) {
      if (!byId[id]) {
        byId[id] = { id: id, name: name || id, fields: [] };
        order.push(byId[id]);
      }
      return byId[id];
    }

    defs.forEach(function (g) { bucket(g.id, g.name); });
    schema.fields.forEach(function (f) {
      bucket(f.group || defs[0].id, f.group).fields.push(f);
    });

    order.forEach(function (b) {
      if (!b.fields.length) return;
      var section = el('div', { class: 'ing-group' });
      // Заголовок — кнопка сворачивания: палитра из 9 групп и 70+
      // ингредиентов целиком помещается только в прокручиваемом списке,
      // а сворачивание убирает из него лишние группы.
      var head = el('button', {
        type: 'button',
        class: 'ing-group-head',
        'aria-expanded': 'true'
      }, [
        el('span', { class: 'caret', 'aria-hidden': 'true' }),
        el('span', { class: 'ing-group-name', text: b.name }),
        el('span', { class: 'ing-group-count', text: String(b.fields.length) })
      ]);
      head.addEventListener('click', function () {
        var collapsed = section.classList.toggle('collapsed');
        head.setAttribute('aria-expanded', collapsed ? 'false' : 'true');
      });
      section.appendChild(head);
      var list = el('div', { class: 'ing-list' });
      list.setAttribute('data-group-id', b.id);
      b.fields.forEach(function (f) { list.appendChild(ingredientCard(f)); });
      section.appendChild(list);
      box.appendChild(section);
    });

    applySearch(els['e-search'] ? els['e-search'].value : '');
  }

  /* Компактная карточка: icon + название + drag handle. */
  function ingredientCard(f) {
    var card = el('div', { class: 'ing', title: f.title + ' (' + f.nspName + ')' });
    card.setAttribute('data-chip-field', f.id);
    card.appendChild(el('span', { class: 'ing-icon t-' + f.valueType, text: typeGlyph(f.valueType) }));
    card.appendChild(el('span', { class: 'ing-title', text: f.title }));
    if (isPersonal(f.id)) {
      card.appendChild(el('span', { class: 'ing-flag', text: 'личное', title: 'Персональное поле' }));
    }
    card.appendChild(el('span', { class: 'ing-handle', text: '⠿', 'aria-hidden': 'true' }));
    // Клик и drag обрабатываются делегированием (bindPaletteClicks,
    // initDrag): слушатели на самой карточке не переживают перерисовку
    // палитры, а во время drag карточка вообще не двигается в DOM.
    return card;
  }

  /* Клик по ингредиенту — доступная альтернатива перетаскиванию.
   * Делегирование на всём поле: клик работает и после drag — флаг
   * suppressClick гасит только клик, которым закончился drag. */
  function bindPaletteClicks() {
    var box = els['e-palette'];
    if (!box || box.getAttribute('data-click-bound') === '1') return;
    box.setAttribute('data-click-bound', '1');
    box.addEventListener('click', function (e) {
      if (!editable) return;
      if (suppressClick) { suppressClick = false; return; }
      var node = e.target;
      var card = null;
      while (node && node !== box) {
        if (node.classList && node.classList.contains('ing')) { card = node; break; }
        node = node.parentNode;
      }
      if (!card) return;
      var field = card.getAttribute('data-chip-field');
      if (!field) return;
      recomputeSelected();
      insertIntoGroup(selectedGroupId, null, defaultCond(field));
      renderTree();
      scheduleValidate(0);
    });
  }

  function bindSearch() {
    var search = els['e-search'];
    if (!search) return;
    search.addEventListener('input', function () { applySearch(search.value); });
  }

  function applySearch(query) {
    var box = els['e-palette'];
    if (!box) return;
    var q = (query || '').trim().toLowerCase();
    // В режиме поиска совпадения показываются даже в свёрнутых группах
    // (см. #e-palette.searching в muzlovar.css); состояние свёрнутости
    // при этом сохраняется и возвращается после очистки поиска.
    box.classList.toggle('searching', !!q);
    box.querySelectorAll('.ing').forEach(function (card) {
      var title = card.querySelector('.ing-title');
      var hit = !q || (title && title.textContent.toLowerCase().indexOf(q) >= 0);
      card.hidden = !hit;
    });
    box.querySelectorAll('.ing-group').forEach(function (group) {
      var shown = false;
      group.querySelectorAll('.ing').forEach(function (c) { if (!c.hidden) shown = true; });
      group.hidden = !shown;
    });
  }

  /* ------------------------------------------------------------------ */
  /* Рендер                                                              */
  /* ------------------------------------------------------------------ */

  function renderAll() {
    els['e-name'].value = model.name || '';
    els['e-desc'].value = model.description || '';
    els['e-public'].checked = !!model.public;
    els['e-limit'].value = (model.limit === null || model.limit === undefined) ? '' : String(model.limit);
    renderTree();
    renderSort();
    updatePreviewCard();
  }

  function renderTree() {
    var box = els['e-tree'];
    if (!box) return;
    ensureIds(model);
    recomputeSelected();
    clear(box);
    box.appendChild(renderGroup(model.root, '/root', 0));
    renderRulesView();
  }

  function groupHead(group, path, depth) {
    var head = el('div', { class: 'group-head' });

    head.appendChild(el('span', {
      class: 'drag-handle', text: '⠿', title: 'Перетащить группу', 'aria-hidden': 'true'
    }));

    var sel = el('select', {
      'aria-label': 'Логика группы',
      onchange: function (e) {
        group.kind = e.target.value;
        renderTree();
        scheduleValidate();
      }
    });
    schema.groupKinds.forEach(function (k) {
      var o = el('option', { value: k.id, text: k.name });
      if (k.id === group.kind) o.selected = true;
      sel.appendChild(o);
    });
    head.appendChild(sel);
    head.appendChild(el('span', { class: 'count', text: group.items.length + ' элем.' }));

    if (path !== '/root') {
      head.appendChild(el('button', {
        class: 'small ghost danger quiet', type: 'button', text: '✕',
        title: 'Убрать группу', 'aria-label': 'Убрать группу',
        disabled: editable ? null : 'disabled',
        onclick: function () {
          removeItemAt(path);
          selectedGroupId = model.root.id;
          renderTree(); scheduleValidate(0);
        }
      }));
    }

    head.appendChild(el('span', { class: 'spacer' }));
    var picked = selectedGroupId === group.id;
    head.appendChild(el('button', {
      class: 'small ghost quiet pick' + (picked ? ' on' : ''),
      type: 'button',
      text: picked ? '✓ выбрана' : 'Выбрать',
      'aria-pressed': picked ? 'true' : 'false',
      title: 'Клик по ингредиенту добавит условие в конец этой группы',
      onclick: function () {
        selectedGroupId = group.id;
        toast('ok', 'Группа выбрана: клик по ингредиенту добавит условие сюда.');
        renderTree();
      }
    }));
    head.appendChild(el('button', {
      class: 'small add', type: 'button', text: '+ группа',
      disabled: editable ? null : 'disabled',
      onclick: function () {
        var sub = { id: newId(), type: 'group', kind: 'any', items: [] };
        group.items.push(sub);
        selectedGroupId = sub.id;
        renderTree(); scheduleValidate(0);
      }
    }));
    head.appendChild(el('button', {
      class: 'small add', type: 'button', text: '+ условие',
      disabled: editable ? null : 'disabled',
      onclick: function () {
        group.items.push(defaultCond(schema.fields[0].id));
        renderTree(); scheduleValidate(0);
      }
    }));

    return head;
  }

  function renderGroup(group, path, depth) {
    var kindCls = group.kind === 'any' ? 'kind-any' : 'kind-all';
    var depthCls = depth === 0 ? 'group-root' : 'group-nested';
    var wrap = el('div', { class: 'group ' + kindCls + ' ' + depthCls });
    wrap.setAttribute('data-group-path', path);
    wrap.appendChild(groupHead(group, path, depth));

    var ul = el('ul', { class: 'tree' });
    ul.setAttribute('data-list-path', path);
    // Группа-приёмник переноса определяется по стабильному id:
    // индексный путь (data-list-path) — только для отладки и тестов.
    ul.setAttribute('data-list-id', group.id || '');

    group.items.forEach(function (item, i) {
      var p = path + '/items/' + i;
      var li = el('li', { class: 'node' });
      li.setAttribute('data-path', p);
      li.setAttribute('data-id', item.id || '');
      if (item.type === 'group') {
        li.appendChild(renderGroup(item, p, depth + 1));
      } else if (item.type === 'raw') {
        li.appendChild(renderRaw(item, p));
      } else {
        li.appendChild(renderCond(item, p, i, group));
      }
      ul.appendChild(li);
    });

    wrap.appendChild(ul);
    return wrap;
  }

  function renderRaw(item, path) {
    var box = el('div', { class: 'cond raw' });
    box.appendChild(el('span', {
      class: 'drag-handle', text: '⠿', title: 'Перетащить', 'aria-hidden': 'true'
    }));
    box.appendChild(el('span', { class: 'raw-note', text: 'внешний узел' }));
    var text = '';
    try { text = JSON.stringify(item.raw); } catch (e) { text = String(item.raw); }
    box.appendChild(el('code', { class: 'raw-note', text: text.slice(0, 160) }));
    box.appendChild(el('span', {
      class: 'type-label',
      text: 'Не выражимо в DSL — доступно только чтение.'
    }));
    if (editable) {
      box.appendChild(el('button', {
        class: 'small danger quiet', type: 'button', text: 'Удалить',
        title: 'Удалить неизвестный узел',
        onclick: function () { removeItemAt(path); renderTree(); scheduleValidate(0); }
      }));
    }
    return box;
  }

  function renderCond(item, path, index, group) {
    var box = el('div', { class: 'cond' });
    var f = fieldById(item.field) || schema.fields[0];

    // drag handle слева — параметры inline, удаление справа.
    box.appendChild(el('span', {
      class: 'drag-handle', text: '⠿', title: 'Перетащить', 'aria-hidden': 'true'
    }));

    var fieldSel = el('select', {
      'aria-label': 'Поле',
      'class': 'field-sel',
      disabled: editable ? null : 'disabled',
      onchange: function (e) {
        item.field = e.target.value;
        var nf = fieldById(item.field);
        var ids = opIds(nf);
        if (ids.indexOf(item.op) < 0) item.op = ids[0];
        item.value = defaultValue(nf, item.op);
        renderTree(); scheduleValidate(0);
      }
    });
    schema.fields.forEach(function (sf) {
      var o = el('option', { value: sf.id, text: sf.title, title: sf.nspName });
      if (sf.id === item.field) o.selected = true;
      fieldSel.appendChild(o);
    });
    box.appendChild(fieldSel);

    if (isPersonal(f.id)) {
      box.appendChild(el('span', { class: 'badge personal', title: 'Персональное поле', text: 'личное' }));
    }

    var opSel = el('select', {
      'aria-label': 'Оператор',
      disabled: editable ? null : 'disabled',
      onchange: function (e) {
        item.op = e.target.value;
        item.value = defaultValue(f, item.op);
        renderTree(); scheduleValidate(0);
      }
    });
    opIds(f).forEach(function (oid) {
      var op = operatorById(oid);
      var o = el('option', {
        value: oid,
        text: op ? op.name : oid,
        title: op ? (op.name + ' — ' + op.hint) : oid
      });
      if (oid === item.op) o.selected = true;
      opSel.appendChild(o);
    });
    box.appendChild(opSel);

    if (needsValue(item.op)) {
      box.appendChild(valueEditor(f, item));
    } else {
      item.value = null;
      box.appendChild(el('span', { class: 'type-label', text: '(без значения)' }));
    }

    var move = el('div', { class: 'move' });
    ['↑', '↓'].forEach(function (label, k) {
      move.appendChild(el('button', {
        class: 'small ghost', type: 'button', text: label,
        'aria-label': (k === 0 ? 'Переместить вверх' : 'Переместить вниз'),
        title: (k === 0 ? 'Переместить вверх' : 'Переместить вниз'),
        disabled: editable ? null : 'disabled',
        onclick: function () {
          var i = group.items.indexOf(item);
          var j = k === 0 ? i - 1 : i + 1;
          if (j < 0 || j >= group.items.length) return;
          group.items.splice(i, 1);
          group.items.splice(j, 0, item);
          renderTree(); scheduleValidate(0);
        }
      }));
    });
    if (editable) {
      move.appendChild(el('button', {
        class: 'small danger quiet', type: 'button', text: '✕',
        'aria-label': 'Удалить условие', title: 'Удалить условие',
        onclick: function () { removeItemAt(path); renderTree(); scheduleValidate(0); }
      }));
    }
    box.appendChild(move);
    return box;
  }

  function valueEditor(field, item) {
    var vt = opValueType(field, item.op);

    if (vt === 'numberRange') {
      if (!Array.isArray(item.value) || item.value.length !== 2) item.value = [0, 100];
      item.value[0] = clampNum(field, item.value[0]);
      item.value[1] = clampNum(field, item.value[1]);
      var wrap = el('span', { class: 'type-label', text: 'между' });
      var lo = el('input', addNumAttrs({
        type: 'number', value: String(item.value[0]), style: 'width:8ch',
        'aria-label': 'Нижняя граница',
        disabled: editable ? null : 'disabled',
        oninput: function (e) {
          item.value[0] = clampNum(field, parseFloat(e.target.value) || 0);
          scheduleValidate();
        },
        onchange: function (e) { e.target.value = String(item.value[0]); }
      }, field));
      var hi = el('input', addNumAttrs({
        type: 'number', value: String(item.value[1]), style: 'width:8ch',
        'aria-label': 'Верхняя граница',
        disabled: editable ? null : 'disabled',
        oninput: function (e) {
          item.value[1] = clampNum(field, parseFloat(e.target.value) || 0);
          scheduleValidate();
        },
        onchange: function (e) { e.target.value = String(item.value[1]); }
      }, field));
      wrap.appendChild(lo);
      wrap.appendChild(document.createTextNode(' и '));
      wrap.appendChild(hi);
      return wrap;
    }

    if (vt === 'dateRange') {
      if (!Array.isArray(item.value) || item.value.length !== 2 ||
          !isDay(item.value[0]) || !isDay(item.value[1])) {
        item.value = [todayIso(), todayIso()];
      }
      var dwrap = el('span', { class: 'type-label', text: 'между' });
      dwrap.appendChild(dateInput(item.value[0], function (v) { item.value[0] = v; }, 'Нижняя граница'));
      dwrap.appendChild(document.createTextNode(' и '));
      dwrap.appendChild(dateInput(item.value[1], function (v) { item.value[1] = v; }, 'Верхняя граница'));
      return dwrap;
    }

    if (vt === 'days') {
      if (typeof item.value !== 'number') item.value = 30;
      return el('span', { class: 'type-label', text: 'за N дней:' }, [
        el('input', {
          type: 'number', min: '1', value: String(item.value), style: 'width:8ch;margin-left:6px',
          'aria-label': 'Количество дней',
          disabled: editable ? null : 'disabled',
          oninput: function (e) { item.value = parseInt(e.target.value, 10) || 0; scheduleValidate(); }
        })
      ]);
    }

    if (vt === 'date') {
      if (!isDay(item.value)) item.value = todayIso();
      return dateInput(item.value, function (v) { item.value = v; });
    }

    /* Ссылка на подборку: выбор вида (id/путь к файлу) и строка
     * значения. Список видов приходит из схемы (field.refKinds) —
     * фронтенд не хранит собственных списков. */
    if (vt === 'playlistRef') {
      var kinds = (field && field.refKinds) || [];
      if (!item.value || typeof item.value !== 'object') item.value = {};
      if (!kinds.some(function (k) { return k.id === item.value.kind; })) {
        item.value.kind = kinds.length ? kinds[0].id : '';
      }
      if (typeof item.value.value !== 'string') item.value.value = '';
      var rwrap = el('span', {});
      var ksel = el('select', {
        'aria-label': 'Вид ссылки',
        disabled: editable ? null : 'disabled',
        onchange: function (e) { item.value.kind = e.target.value; scheduleValidate(); }
      });
      kinds.forEach(function (k) {
        var o = el('option', { value: k.id, text: k.label });
        if (item.value.kind === k.id) o.selected = true;
        ksel.appendChild(o);
      });
      rwrap.appendChild(ksel);
      rwrap.appendChild(el('input', {
        type: 'text', value: item.value.value, style: 'width:18ch;margin-left:6px',
        'aria-label': 'Значение ссылки',
        disabled: editable ? null : 'disabled',
        oninput: function (e) { item.value.value = e.target.value; scheduleValidate(); }
      }));
      return rwrap;
    }

    if (vt === 'bool') {
      var sel = el('select', {
        'aria-label': 'Значение',
        disabled: editable ? null : 'disabled',
        onchange: function (e) { item.value = e.target.value === 'true'; scheduleValidate(); }
      });
      var variants = field.valueVariants || ['да', 'нет'];
      ['true', 'false'].forEach(function (v, i) {
        var o = el('option', { value: v, text: variants[i] || v });
        if (String(!!item.value) === v) o.selected = true;
        sel.appendChild(o);
      });
      return sel;
    }

    if (vt === 'number') {
      if (typeof item.value !== 'number') item.value = 0;
      item.value = clampNum(field, item.value);
      return el('input', addNumAttrs({
        type: 'number', value: String(item.value), style: 'width:12ch',
        'aria-label': 'Числовое значение',
        disabled: editable ? null : 'disabled',
        oninput: function (e) {
          item.value = clampNum(field, parseFloat(e.target.value) || 0);
          scheduleValidate();
        },
        onchange: function (e) { e.target.value = String(item.value); }
      }, field));
    }

    /* Закрытый набор значений (enum из схемы): обычный select, никакой
     * логики вручную — подписи и значения приходят из /api/schema. */
    if (field.enum && field.enum.length) {
      var known = field.enum.some(function (v) { return v.value === item.value; });
      if (typeof item.value !== 'string' || !known) item.value = field.enum[0].value;
      var esel = el('select', {
        'aria-label': 'Значение',
        disabled: editable ? null : 'disabled',
        onchange: function (e) { item.value = e.target.value; scheduleValidate(); }
      });
      field.enum.forEach(function (v) {
        var o = el('option', { value: v.value, text: v.label });
        if (item.value === v.value) o.selected = true;
        esel.appendChild(o);
      });
      return esel;
    }

    if (typeof item.value !== 'string') item.value = '';
    return el('input', {
      type: 'text', value: item.value, style: 'width:18ch',
      'aria-label': 'Текстовое значение',
      disabled: editable ? null : 'disabled',
      oninput: function (e) { item.value = e.target.value; scheduleValidate(); }
    });
  }

  /* ------------------------------------------------------------------ */
  /* Дерево условия — предпросмотр (только чтение)                       */
  /* ------------------------------------------------------------------ */

  function renderRulesView() {
    var box = els['e-rules'];
    if (!box || !model || !schema) return;
    clear(box);
    box.appendChild(viewGroup(model.root));
  }

  function viewGroup(group) {
    var any = group.kind === 'any';
    var wrap = el('div', { class: 'vgroup ' + (any ? 'kind-any' : 'kind-all') });
    wrap.appendChild(el('div', { class: 'vgroup-head' }, [
      el('span', { class: 'vk', text: any ? 'ЛЮБОЕ' : 'ВСЕ' }),
      el('span', { class: 'vcount', text: group.items.length + ' элем.' })
    ]));

    var list = el('div', { class: 'vlist' + (group.items.length ? '' : ' plain') });
    if (!group.items.length) {
      list.appendChild(el('div', { class: 'vempty', text: 'нет условий' }));
    }
    group.items.forEach(function (item) {
      if (item.type === 'group') {
        list.appendChild(viewGroup(item));
        return;
      }
      if (item.type === 'raw') {
        list.appendChild(el('div', { class: 'vcond', text: 'внешний узел — только чтение' }));
        return;
      }
      var f = fieldById(item.field);
      var op = operatorById(item.op);
      var row = el('div', { class: 'vcond' }, [
        el('span', { class: 'vfield', text: f ? f.title : item.field }),
        el('span', { class: 'vop', text: op ? op.name : item.op })
      ]);
      var value = condValueText(f, item);
      if (value) row.appendChild(el('span', { class: 'vval', text: value }));
      if (f && isPersonal(f.id)) row.appendChild(el('span', { class: 'badge personal', text: 'личное' }));
      list.appendChild(row);
    });

    wrap.appendChild(list);
    return wrap;
  }

  /* ------------------------------------------------------------------ */
  /* Сортировка и лимит                                                  */
  /* ------------------------------------------------------------------ */

  function renderSort() {
    var box = els['e-sort'];
    if (!box) return;
    clear(box);

    var modeSel = el('select', {
      'aria-label': 'Режим сортировки',
      disabled: editable ? null : 'disabled',
      onchange: function (e) {
        var v = e.target.value;
        if (v === 'random') model.sort = { kind: 'random' };
        else if (v === 'none') model.sort = null;
        else if (!model.sort || model.sort.kind !== 'fields') model.sort = { kind: 'fields', items: [] };
        renderSort(); scheduleValidate(0);
      }
    });
    [['random', 'случайно'], ['fields', 'по полям'], ['none', 'по умолчанию']].forEach(function (pair) {
      var cur = model.sort ? model.sort.kind : 'none';
      var o = el('option', { value: pair[0], text: pair[1] });
      if (pair[0] === cur) o.selected = true;
      modeSel.appendChild(o);
    });
    box.appendChild(modeSel);

    if (model.sort && model.sort.kind === 'raw') {
      box.appendChild(el('div', {
        class: 'state warn',
        text: 'Сортировка из внешнего файла не поддерживается редактором: ' + model.sort.text
      }));
      return;
    }

    if (model.sort && model.sort.kind === 'fields') {
      if (!model.sort.items.length) {
        model.sort.items = [{ field: schema.sortFields[0], dir: 'asc' }];
      }
      var list = el('div', { class: 'palette' });
      model.sort.items.forEach(function (s, i) {
        var row = el('span', { class: 'cond' });
        var fs = el('select', {
          'aria-label': 'Поле сортировки',
          disabled: editable ? null : 'disabled',
          onchange: function (e) { s.field = e.target.value; scheduleValidate(); }
        });
        schema.sortFields.forEach(function (id) {
          var f = fieldById(id);
          var o = el('option', { value: id, text: f ? f.title : id });
          if (id === s.field) o.selected = true;
          fs.appendChild(o);
        });
        row.appendChild(fs);
        var ds = el('select', {
          'aria-label': 'Направление',
          disabled: editable ? null : 'disabled',
          onchange: function (e) { s.dir = e.target.value; scheduleValidate(); }
        });
        schema.sortDirections.forEach(function (d) {
          var o = el('option', { value: d.id, text: d.name });
          if (d.id === s.dir) o.selected = true;
          ds.appendChild(o);
        });
        row.appendChild(ds);
        row.appendChild(el('button', {
          class: 'small danger quiet', type: 'button', text: '✕',
          'aria-label': 'Убрать поле сортировки',
          disabled: editable ? null : 'disabled',
          onclick: function () {
            model.sort.items.splice(i, 1);
            if (!model.sort.items.length) model.sort = { kind: 'random' };
            renderSort(); scheduleValidate(0);
          }
        }));
        list.appendChild(row);
      });
      box.appendChild(list);
      box.appendChild(el('button', {
        class: 'small add', type: 'button', text: '+ поле',
        disabled: editable ? null : 'disabled',
        onclick: function () {
          model.sort.items.push({ field: schema.sortFields[0], dir: 'asc' });
          renderSort(); scheduleValidate(0);
        }
      }));
    }
  }

  /* ------------------------------------------------------------------ */
  /* Drag & drop                                                         */
  /* ------------------------------------------------------------------ */
  /*
   * Перенос выполняется без единого изменения DOM дерева во время
   * drag: курсор тянет фиксированный «призрак» (клон исходного узла,
   * position: fixed), исходный узел лишь приглушается, а позицию
   * вставки показывает отдельный индикатор — layout под курсором
   * неподвижен, поэтому точка drop не «уезжает».
   *
   * Модель меняется ровно один раз — в pointerup — и только по
   * стабильным id: элемент берётся по item.id, группа-приёмник — по
   * group.id, а позиция задаётся id соседа «перед которым вставить»
   * (beforeId, null — в конец). Индексные пути и названия полей в
   * расчёте участвуют только как отладочные метки. Это убирает
   * прежний конфликт: SortableJS переставлял узлы (и переносил клон
   * карточки палитры) прямо во время drag, после чего onAdd/onEnd и
   * полный re-render спорили об одних и тех же узлах и индексах.
   */

  var drag = null;           // активный перенос
  var pendingDrag = null;    // нажатие, ещё не ставшее переносом
  var suppressClick = false; // клик, которым закончился drag, не добавляет условие

  function initDrag() {
    document.addEventListener('pointerdown', onDragPointerDown);
    document.addEventListener('pointermove', onDragPointerMove);
    document.addEventListener('pointerup', onDragPointerUp);
    document.addEventListener('pointercancel', function () { endDrag(); });
    document.addEventListener('keydown', function (e) {
      if (e.key !== 'Escape') return;
      pendingDrag = null;
      if (drag) endDrag();
    });
  }

  /* Источник переноса: карточка палитры целиком либо drag handle
   * дерева — тогда узел определяется по ближайшему li[data-id]. */
  function dragSourceAt(node) {
    var n = node;
    while (n && n.nodeType === 1) {
      if (n.classList.contains('ing')) {
        var field = n.getAttribute('data-chip-field');
        return field ? { type: 'chip', field: field, el: n } : null;
      }
      if (n.classList.contains('drag-handle')) {
        var li = n.parentNode;
        while (li && li.nodeType === 1 &&
               !(li.classList.contains('node') && li.getAttribute('data-id'))) {
          li = li.parentNode;
        }
        if (li && li.nodeType === 1 && li.getAttribute('data-id')) {
          return { type: 'item', id: li.getAttribute('data-id'), el: li };
        }
        return null; // handle корневой группы: переносить нечего
      }
      n = n.parentNode;
    }
    return null;
  }

  function onDragPointerDown(e) {
    suppressClick = false;
    if (drag || pendingDrag || !editable || !model || !schema) return;
    if (e.pointerType === 'mouse' && e.button !== 0) return;
    var src = dragSourceAt(e.target);
    if (!src) return;
    var r = src.el.getBoundingClientRect();
    pendingDrag = {
      type: src.type,
      field: src.field,
      id: src.id,
      el: src.el,
      pointerId: e.pointerId,
      startX: e.clientX,
      startY: e.clientY,
      grabX: e.clientX - r.left,
      grabY: e.clientY - r.top,
      width: r.width
    };
  }

  function onDragPointerMove(e) {
    if (drag) {
      if (e.pointerId !== drag.pointerId) return;
      e.preventDefault();
      drag.lastX = e.clientX;
      drag.lastY = e.clientY;
      autoScroll(e.clientX, e.clientY);
      refreshTarget(e.clientX, e.clientY);
      moveGhost(e.clientX, e.clientY);
      return;
    }
    if (!pendingDrag || e.pointerId !== pendingDrag.pointerId) return;
    var dx = e.clientX - pendingDrag.startX;
    var dy = e.clientY - pendingDrag.startY;
    if (dx * dx + dy * dy < 36) return; // порог 6px: меньше — это клик
    startDrag(e.clientX, e.clientY);
  }

  function startDrag(x, y) {
    var src = pendingDrag;
    pendingDrag = null;
    if (!src || !src.el.isConnected) return;

    var ghost = src.el.cloneNode(true);
    ghost.classList.add('drag-ghost');
    ghost.style.width = src.width + 'px';
    stripIdentity(ghost);
    document.body.appendChild(ghost);

    var indicator = document.createElement('div');
    indicator.className = 'drop-indicator hidden';
    indicator.setAttribute('aria-hidden', 'true');
    document.body.appendChild(indicator);

    if (src.type === 'item') src.el.classList.add('drag-source');

    drag = {
      type: src.type,
      field: src.field,
      id: src.id,
      el: src.el,
      pointerId: src.pointerId,
      grabX: src.grabX,
      grabY: src.grabY,
      ghost: ghost,
      indicator: indicator,
      hoverGroup: null,
      target: null,
      lastX: x,
      lastY: y
    };
    suppressClick = true;
    document.body.classList.add('dragging');
    window.addEventListener('scroll', onDragViewportChange, true);
    window.addEventListener('resize', onDragViewportChange);
    moveGhost(x, y);
    refreshTarget(x, y);
  }

  /* В призраке не остаётся служебных атрибутов идентичности: во
   * время тестов и отладки в документе нет второго узла с тем же id. */
  function stripIdentity(root) {
    var nodes = [root];
    if (root.querySelectorAll) {
      Array.prototype.forEach.call(root.querySelectorAll('[data-id]'), function (n) { nodes.push(n); });
    }
    nodes.forEach(function (n) {
      ['data-id', 'data-path', 'data-list-id', 'data-list-path', 'data-chip-field']
        .forEach(function (a) { n.removeAttribute(a); });
    });
  }

  function moveGhost(x, y) {
    drag.ghost.style.left = (x - drag.grabX) + 'px';
    drag.ghost.style.top = (y - drag.grabY) + 'px';
  }

  function onDragPointerUp(e) {
    if (!drag) { pendingDrag = null; return; }
    if (e.pointerId !== drag.pointerId) return;
    var t = drag.target;
    applyDrop(t);
    endDrag();
  }

  function endDrag() {
    if (!drag) { pendingDrag = null; return; }
    window.removeEventListener('scroll', onDragViewportChange, true);
    window.removeEventListener('resize', onDragViewportChange);
    removeNode(drag.ghost);
    removeNode(drag.indicator);
    if (drag.hoverGroup) drag.hoverGroup.classList.remove('drop-hover');
    if (drag.el && drag.el.classList) drag.el.classList.remove('drag-source');
    document.body.classList.remove('dragging');
    drag = null;
    pendingDrag = null;
  }

  function onDragViewportChange() {
    if (drag) refreshTarget(drag.lastX, drag.lastY);
  }

  function removeNode(n) {
    if (n && n.parentNode) n.parentNode.removeChild(n);
  }

  /* Автопрокрутка колонки дерева, когда курсор у края.
   *
   * Только когда курсор реально над колонкой: drag начинается на
   * карточке палитры (она у нижнего края окна), и без проверки по X
   * дерево прокручивалось на 14px при старте — цель вставки уезжала
   * в другую группу. */
  function autoScroll(x, y) {
    var host = els['e-tree'] ? els['e-tree'].parentElement : null;
    if (!host) return;
    var r = host.getBoundingClientRect();
    if (x < r.left || x > r.right) return;
    if (y < r.top + 28) host.scrollTop -= 14;
    else if (y > r.bottom - 28) host.scrollTop += 14;
  }

  function refreshTarget(x, y) {
    var t = resolveTarget(x, y);
    var group = t ? t.groupEl : null;
    if (drag.hoverGroup !== group) {
      if (drag.hoverGroup) drag.hoverGroup.classList.remove('drop-hover');
      drag.hoverGroup = group;
      if (group) group.classList.add('drop-hover');
    }
    drag.target = t;
    if (!t) { drag.indicator.classList.add('hidden'); return; }
    drag.indicator.classList.remove('hidden');
    positionIndicator(t);
  }

  /* Точка вставки из координат курсора.
   *
   * Цель — самая вложенная группа, содержащая точку: её собственный
   * ul.tree (шапка вложенной группы попадает в неё же, то есть drag
   * на шапку вставляет в начало этой группы; шапка корня — в начало
   * корня). Позиция — id первого узла списка, середина которого ниже
   * курсора; если это сам переносимый узел, для модели берётся id его
   * прежнего следующего соседа (после снятия узла «перед ним» и
   * «перед своим следующим» — одна и та же позиция). */
  function resolveTarget(x, y) {
    var hit = document.elementFromPoint(x, y);
    if (!hit || !hit.closest) return null;
    var groupEl = hit.closest('.group');
    if (!groupEl) return null;
    var gr = groupEl.getBoundingClientRect();
    if (x < gr.left || x > gr.right || y < gr.top || y > gr.bottom) return null;
    var ul = groupEl.querySelector(':scope > ul.tree');
    if (!ul) return null;
    var groupId = ul.getAttribute('data-list-id');
    if (!groupId || !findGroupById(groupId)) return null;
    // Группа не может переноситься внутрь себя или своего потомка.
    if (drag.type === 'item' && groupIsInsideItem(drag.id, groupId)) return null;

    var visualId = null;
    var children = ul.children;
    for (var i = 0; i < children.length; i++) {
      var ch = children[i];
      if (!ch.classList || !ch.classList.contains('node')) continue;
      var cr = ch.getBoundingClientRect();
      if (y < cr.top + cr.height / 2) { visualId = ch.getAttribute('data-id'); break; }
    }
    var beforeId = visualId;
    if (drag.type === 'item' && beforeId === drag.id) {
      beforeId = nextSiblingItemId(ul, drag.el);
    }
    return { groupId: groupId, beforeId: beforeId, visualId: visualId, listEl: ul, groupEl: groupEl };
  }

  function nextSiblingItemId(ul, el) {
    var n = el && el.parentNode === ul ? el.nextElementSibling : null;
    while (n && !(n.classList && n.classList.contains('node'))) n = n.nextElementSibling;
    return n ? (n.getAttribute('data-id') || null) : null;
  }

  function childNodeById(ul, id) {
    var children = ul.children;
    for (var i = 0; i < children.length; i++) {
      var ch = children[i];
      if (ch.classList && ch.classList.contains('node') && ch.getAttribute('data-id') === id) return ch;
    }
    return null;
  }

  function lastNodeChild(ul) {
    var children = ul.children;
    for (var i = children.length - 1; i >= 0; i--) {
      var ch = children[i];
      if (ch.classList && ch.classList.contains('node')) return ch;
    }
    return null;
  }

  /* Индикатор — линия ровно на границе вставки; сам он зафиксирован
   * в viewport и ничего не сдвигает. */
  function positionIndicator(t) {
    var lr = t.listEl.getBoundingClientRect();
    var y = null;
    if (t.visualId) {
      var c = childNodeById(t.listEl, t.visualId);
      if (c) y = c.getBoundingClientRect().top - 8; // середина зазора
    }
    if (y === null) {
      var last = lastNodeChild(t.listEl);
      y = last ? last.getBoundingClientRect().bottom + 8 : lr.top + 1;
    }
    drag.indicator.style.left = lr.left + 'px';
    drag.indicator.style.width = lr.width + 'px';
    drag.indicator.style.top = (y - 1) + 'px';
  }

  /* Применение переноса к модели — один раз, в pointerup. */
  function applyDrop(t) {
    if (!t || !editable || !model) return;

    if (drag.type === 'chip') {
      if (!insertIntoGroup(t.groupId, t.beforeId, defaultCond(drag.field))) return;
      renderTree();
      scheduleValidate(0);
      return;
    }

    var cur = findItem(drag.id);
    if (!cur) return;
    var target = findGroupById(t.groupId);
    if (!target) return;
    var nextId = cur.index + 1 < cur.list.length ? cur.list[cur.index + 1].id : null;
    // Drop на прежнюю позицию — модель не меняется, DOM не трогаем.
    if (cur.list === target.items && t.beforeId === nextId) return;
    var moved = removeItemById(drag.id);
    if (!moved) return;
    if (!insertIntoGroup(t.groupId, t.beforeId, moved)) {
      cur.list.splice(Math.min(cur.index, cur.list.length), 0, moved);
      return;
    }
    renderTree();
    scheduleValidate(0);
  }

  /* ------------------------------------------------------------------ */
  /* Валидация и предпросмотр                                            */
  /* ------------------------------------------------------------------ */

  /* Единственная точка запуска проверки: её вызывает ЛЮБОЕ изменение
   * рецепта (поле, дерево, сортировка, лимит), поэтому статус, ошибки
   * и предупреждения обновляются сами — ручная кнопка «Проверить»
   * здесь не нужна. debounce: 400 мс набора текста, 0 — после
   * структурных изменений. */
  function scheduleValidate(delay) {
    if (!schema) return;
    // Живое обновление вкладки «Правила»: локальный рендер не ждёт
    // ответа сервера и выполняется при любом изменении рецепта.
    renderRulesView();
    if (validateTimer) clearTimeout(validateTimer);
    validateTimer = setTimeout(runValidate,
      delay === undefined ? 400 : delay);
  }

  function setValidity(kind, text) {
    var box = els['e-validity'];
    if (!box) return;
    box.className = 'validity ' + kind;
    if (els['e-validity-text']) els['e-validity-text'].textContent = text;
  }

  /* Один проход валидации. Возвращает Promise<bool> («модель
   * валидна») и кладёт результат в lastValidateOk — его же читает
   * публикация. */
  function runValidate() {
    if (!model) return Promise.resolve(false);
    validateTimer = null;
    setValidity('pending', 'Проверяем…');
    return api('/api/validate', { method: 'POST', body: model }).then(function (res) {
      var errs = errorsOf(res.data);
      var warns = (res.data && Array.isArray(res.data.warnings)) ? res.data.warnings : [];
      lastValidateOk = res.ok && !errs.length;
      renderErrors(els['e-errors'], errs,
        lastValidateOk ? '' : 'Подборка не прошла валидацию', warns);
      if (lastValidateOk && res.data.mix) {
        els['e-preview'].textContent = res.data.mix;
        els['e-preview'].parentNode.hidden = false;
      } else if (!lastValidateOk) {
        els['e-preview'].textContent = '';
      }
      var nspPre = els['e-preview-nsp'];
      if (nspPre) {
        if (lastValidateOk && res.data.nsp) {
          nspPre.textContent = res.data.nsp;
          if (nspPre.parentNode) nspPre.parentNode.hidden = false;
        } else if (!lastValidateOk) {
          nspPre.textContent = '';
        }
      }
      // Итоговый filename из текущего названия — блок «Путь публикации»
      // показывает ожидающее переименование до публикации.
      if (res.ok && res.data && typeof res.data.slug === 'string') {
        pendingSlug = res.data.slug;
      }
      updatePath();
      if (lastValidateOk) {
        // Предупреждения не блокируют публикацию — только сообщают.
        setValidity(warns.length ? 'warn' : 'ok',
          warns.length
            ? 'Проверено — есть предупреждения: ' + warns.length
            : 'Проверено — можно публиковать');
      } else {
        setValidity('err', 'Ошибки: ' + errs.length);
      }
      return lastValidateOk;
    }).catch(function () {
      renderErrors(els['e-errors'],
        [{ message: 'Сервер недоступен — проверка не выполнена.' }], 'Ошибка');
      lastValidateOk = false;
      setValidity('err', 'Сервер недоступен');
      return false;
    });
  }

  /* Свежий результат проверки перед публикацией: отложенный (debounce)
   * запуск отменяется, валидация выполняется немедленно и её ответ
   * решает, уходит ли запрос на сохранение. Пока debounce висит,
   * lastValidateOk мог устареть относительно уже изменённой модели. */
  function validatedNow() {
    if (!schema || !model) return Promise.resolve(false);
    if (validateTimer) {
      clearTimeout(validateTimer);
      validateTimer = null;
    }
    return runValidate();
  }

  /* ------------------------------------------------------------------ */
  /* Публикация                                                          */
  /* ------------------------------------------------------------------ */

  function publish() {
    // На диск уходит только то, что подтверждено актуальной проверкой;
    // сервер при сохранении выполняет свою проверку независимо.
    validatedNow().then(function (ok) {
      if (!ok) {
        toast('error', 'Сначала исправьте ошибки валидации.');
        return;
      }
      sendPublish(false);
    });
  }

  /* «Сохранить как новую»: текущий рецепт уходит в обычный create
   * (POST без slug) — прежний identity и slug исходной подборки в
   * запрос не попадают, её cleanup/rename не запускается. После
   * успеха редактор переключается на identity новой подборки. */
  function publishAsNew() {
    validatedNow().then(function (ok) {
      if (!ok) {
        toast('error', 'Сначала исправьте ошибки валидации.');
        return;
      }
      sendPublish(false, true);
    });
  }

  function sendPublish(overwrite, asNew) {
    // Как и раньше: обновление идёт по slug открытой подборки, create —
    // без него. «Сохранить как новую» всегда идёт в create, даже когда
    // открытая подборка уже опубликована.
    var create = asNew || !slug;
    var url = create
      ? '/api/playlists' + (overwrite ? '?overwrite=1' : '')
      : '/api/playlists/' + encodeURIComponent(slug) + (overwrite ? '?overwrite=1' : '');
    api(url, { method: create ? 'POST' : 'PUT', body: model }).then(function (res) {
      if (res.ok) {
        toast('ok', asNew
          ? 'Сохранено как новая подборка: ' + (res.data.entry ? res.data.entry.name : '')
          : 'Подборка опубликована: ' + (res.data.entry ? res.data.entry.name : ''));
        var entry = res.data.entry || null;
        var newSlug = entry ? entry.slug : slug;
        if (newSlug && newSlug !== slug) {
          // Переименование (и «Сохранить как новую»): identity подборки
          // и адрес страницы переезжают на новый filename.
          slug = newSlug;
          if (els.editor) els.editor.setAttribute('data-slug', slug);
          window.history.replaceState({}, '', '/edit/' + encodeURIComponent(slug));
        }
        if (res.data.mix) els['e-mix'].value = res.data.mix;
        if (res.data.nsp) els['e-nsp'].value = res.data.nsp;
        renderErrors(els['e-errors'], [], '');
        if (els.editor) {
          els.editor.setAttribute('data-published', '1');
          // После публикации показывается только новый фактический путь.
          var nspName = (entry && entry.nspFile)
            ? entry.nspFile
            : (slug ? slug + '.nsp' : '');
          if (nspName) {
            els.editor.setAttribute('data-published-path', publishFilePath(nspName));
          }
        }
        pendingSlug = slug;
        updatePath();
        syncPublishButtons();
        return;
      }
      if (res.status === 409) {
        var e = errorsOf(res.data)[0] || {};
        if (e.code === 'external_readonly') {
          renderErrors(els['e-errors'], [e], 'Внешняя подборка');
          toast('error', 'Внешнюю подборку нельзя редактировать.');
          return;
        }
        confirmOverwrite(e.message || 'Файлы уже существуют.', asNew);
        return;
      }
      renderErrors(els['e-errors'], errorsOf(res.data), 'Не удалось опубликовать');
      toast('error', 'Публикация не выполнена.');
    }).catch(function () {
      toast('error', 'Сервер недоступен.');
    });
  }

  function confirmOverwrite(message, asNew) {
    var dlg = document.getElementById('overwrite-dialog');
    if (!dlg) { toast('warn', message); return; }
    document.getElementById('overwrite-text').textContent = message;
    dlg.returnValue = '';
    if (typeof dlg.showModal === 'function') dlg.showModal();
    else if (!window.confirm(message)) return;
    dlg.addEventListener('close', function handler() {
      dlg.removeEventListener('close', handler);
      if (dlg.returnValue === 'yes') sendPublish(true, asNew);
    });
  }

  /* ------------------------------------------------------------------ */
  /* Удаление                                                            */
  /* ------------------------------------------------------------------ */

  /* Строки списка: кнопки [data-delete] на серверной таблице. */
  function initList() {
    document.querySelectorAll('[data-delete]').forEach(function (btn) {
      btn.addEventListener('click', function () {
        var target = btn.getAttribute('data-delete');
        var name = btn.getAttribute('data-delete-name') || target;
        var external = btn.getAttribute('data-external') === '1';
        openDeleteDialog(name, !external, function () { doDelete(target); });
      });
    });
  }

  /* Диалог подтверждения: ввод точного названия. Возвращает false,
   * если подтверждение не удалось получить. */
  function openDeleteDialog(name, isEditable, onConfirm) {
    var dlg = document.getElementById('delete-dialog');
    if (!dlg) return false;
    document.getElementById('delete-name').textContent = name;
    document.getElementById('delete-input').value = '';
    document.getElementById('delete-confirm').disabled = true;
    document.getElementById('delete-warning').hidden = !!isEditable;
    if (typeof dlg.showModal !== 'function') {
      if (window.confirm('Удалить подборку «' + name + '»?')) onConfirm();
      return true;
    }
    dlg.showModal();

    var input = document.getElementById('delete-input');
    var btn = document.getElementById('delete-confirm');
    input.oninput = function () { btn.disabled = input.value !== name; };
    btn.onclick = function () {
      dlg.close();
      onConfirm();
    };
    return true;
  }

  function confirmDelete() {
    if (!slug) return;
    openDeleteDialog(model.name || slug, editable, function () { doDelete(slug); });
  }

  function doDelete(target) {
    api('/api/playlists/' + encodeURIComponent(target), { method: 'DELETE' })
      .then(function (res) {
        if (res.ok) {
          var note = 'Перенесено в корзину.';
          if (res.data.subsonic && res.data.subsonic.attempted) {
            note += res.data.subsonic.ok
              ? ' Сущность удалена в Navidrome.'
              : ' Сущность в Navidrome осталась: ' + (res.data.subsonic.message || '');
          } else if (res.data.subsonic) {
            note += ' Сущность в Navidrome нужно удалить вручную (Subsonic не настроен).';
          }
          toast('ok', note);
          window.location.href = '/trash';
        } else if (res.status === 409) {
          toast('error', errorText(res));
        } else {
          renderErrors(els['e-errors'], errorsOf(res.data), 'Не удалось удалить');
          toast('error', errorText(res));
        }
      })
      .catch(function () { toast('error', 'Сервер недоступен.'); });
  }

  /* ------------------------------------------------------------------ */
  /* Корзина                                                             */
  /* ------------------------------------------------------------------ */

  function initTrash() {
    document.querySelectorAll('[data-restore]').forEach(function (btn) {
      btn.addEventListener('click', function () {
        var id = btn.getAttribute('data-restore');
        api('/api/trash/' + encodeURIComponent(id) + '/restore', { method: 'POST', body: {} })
          .then(function (res) {
            if (res.ok) { toast('ok', 'Подборка восстановлена.'); location.reload(); }
            else toast('error', errorText(res));
          })
          .catch(function () { toast('error', 'Сервер недоступен.'); });
      });
    });
    document.querySelectorAll('[data-purge]').forEach(function (btn) {
      btn.addEventListener('click', function () {
        var id = btn.getAttribute('data-purge');
        if (!window.confirm('Удалить запись «' + id + '» из корзины безвозвратно?')) return;
        api('/api/trash/' + encodeURIComponent(id), { method: 'DELETE' })
          .then(function (res) {
            if (res.ok) { toast('ok', 'Запись удалена.'); location.reload(); }
            else toast('error', errorText(res));
          })
          .catch(function () { toast('error', 'Сервер недоступен.'); });
      });
    });
  }
})();
