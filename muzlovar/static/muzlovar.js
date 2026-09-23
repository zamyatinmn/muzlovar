/* Muzlovar — визуальный редактор умных подборок Navidrome.
 *
 * Раскладка desktop: шапка (логотип, статус прода, настройки,
 * «Проверить», «Опубликовать») и три колонки на всю высоту —
 * ингредиенты (~22%), рецепт подборки (~50%) и предпросмотр (~28%).
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
  var editable = true;
  var selectedGroup = '/root';
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
      'e-status', 'e-palette', 'e-tree', 'e-sort', 'e-errors', 'e-preview',
      'e-preview-wrap', 'e-personal', 'e-save', 'e-publish', 'e-delete',
      'e-mix', 'e-nsp', 'toasts',
      'e-search', 'e-rules', 'e-validity', 'e-validity-text',
      'e-path', 'e-path-edit', 'e-copy-mix', 'e-copy-preview',
      'pl-title', 'pl-meta', 'tab-rules', 'tab-mix',
      'conn-status', 'conn-text', 'settings-btn', 'settings-dialog'
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

  function renderErrors(target, errors, heading) {
    if (!target) return;
    clear(target);
    if (!errors.length) { target.hidden = true; return; }
    target.hidden = false;
    target.className = 'state error';
    target.appendChild(el('h3', { text: heading || 'Ошибки' }));
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
    target.appendChild(ul);
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
    initSettings();
    initTabs();
    bindCopy('e-copy-mix', function () { return els['e-mix'] ? els['e-mix'].value : ''; });
    bindCopy('e-copy-preview', function () {
      return els['e-preview'] ? els['e-preview'].textContent : '';
    });
  }

  /* Статус подключения к проду: опрашивает публичный /health. */
  function initConnStatus() {
    var box = els['conn-status'];
    if (!box) return;

    function setConn(ok) {
      box.className = 'conn ' + (ok ? 'ok' : 'down');
      if (els['conn-text']) {
        els['conn-text'].textContent = ok ? 'Прод: подключено' : 'Прод: недоступно';
      }
      box.title = ok
        ? 'Сервер отвечает — прод доступен'
        : 'Сервер не отвечает — проверьте подключение';
    }

    function ping() {
      fetch('/health', { headers: { 'Accept': 'application/json' } })
        .then(function (r) { setConn(r.ok); })
        .catch(function () { setConn(false); });
    }

    ping();
    setInterval(ping, 30000);
  }

  function initSettings() {
    var btn = els['settings-btn'];
    var dlg = els['settings-dialog'];
    if (!btn || !dlg) return;
    btn.addEventListener('click', function () {
      if (typeof dlg.showModal === 'function') dlg.showModal();
    });
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
    ['rules', 'mix'].forEach(function (k) {
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

  function groupAt(path) {
    if (path === '/root') return model.root;
    var parts = path.split('/').slice(1);   // ['root','items','2',...]
    var g = model.root;
    for (var i = 1; i < parts.length; i += 2) {
      if (parts[i] !== 'items') return null;
      var idx = parseInt(parts[i + 1], 10);
      if (!g || !g.items || !g.items[idx]) return null;
      var next = g.items[idx];
      if (!next || next.type !== 'group') return null;
      g = next;
    }
    return g;
  }

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

  function insertItem(groupPath, index, item) {
    var g = groupAt(groupPath);
    if (!g) g = model.root;
    if (index === null || index < 0 || index > g.items.length) g.items.push(item);
    else g.items.splice(index, 0, item);
  }

  function condPath(groupPath, index) {
    return groupPath + '/items/' + index;
  }

  function recomputeSelected() {
    if (!groupAt(selectedGroup)) selectedGroup = '/root';
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
    var opId = f.operators[0];
    return { type: 'cond', field: f.id, op: opId, value: defaultValue(f, opId) };
  }

  function defaultValue(field, op) {
    if (op === 'bare' || op === 'isMissing' || op === 'isPresent') return null;
    if (op === 'between') return [0, 100];
    if (op === 'inTheLast' || op === 'notInTheLast') return 30;
    switch (field.valueType) {
      case 'bool': return false;
      case 'number': return 0;
      default: return '';
    }
  }

  function needsValue(op) {
    return op !== 'bare' && op !== 'isMissing' && op !== 'isPresent';
  }

  /* Текстовое значение условия — для дерева предпросмотра. */
  function condValueText(field, item) {
    if (!needsValue(item.op)) return '';
    if (item.op === 'between') {
      var v = item.value;
      return (Array.isArray(v) && v.length === 2) ? (v[0] + '–' + v[1]) : '';
    }
    if (item.op === 'inTheLast' || item.op === 'notInTheLast') {
      return String(item.value) + ' дн.';
    }
    if (field && field.valueType === 'bool') {
      var variants = field.valueVariants || ['да', 'нет'];
      return item.value ? (variants[0] || 'да') : (variants[1] || 'нет');
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
    ['e-save', 'e-publish', 'e-delete'].forEach(function (id) {
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
    if (els['e-save']) els['e-save'].addEventListener('click', function () { scheduleValidate(0); });
    if (els['e-delete']) els['e-delete'].addEventListener('click', confirmDelete);

    // Путь публикации зависит от названия: «Изменить» ведёт в поле.
    if (els['e-path-edit']) els['e-path-edit'].addEventListener('click', function () {
      var nameInput = els['e-name'];
      if (nameInput) {
        nameInput.focus();
        if (nameInput.select) nameInput.select();
      }
      toast('ok', 'Путь зависит от названия подборки — измените поле «Название».');
    });
  }

  /* Путь публикации: имя файла .nsp в каталоге Navidrome. */
  function updatePath() {
    var p = els['e-path'];
    if (!p) return;
    if (slug) {
      p.textContent = slug + '.nsp';
      p.classList.remove('muted');
    } else {
      p.textContent = 'не опубликована — путь появится после публикации';
      p.classList.add('muted');
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
      section.appendChild(el('div', { class: 'ing-group-head' }, [
        el('span', { class: 'ing-group-name', text: b.name }),
        el('span', { class: 'ing-group-count', text: String(b.fields.length) })
      ]));
      var list = el('div', { class: 'ing-list' });
      list.setAttribute('data-group-id', b.id);
      b.fields.forEach(function (f) { list.appendChild(ingredientCard(f)); });
      section.appendChild(list);
      box.appendChild(section);

      if (window.Sortable) {
        window.Sortable.create(list, {
          group: { name: 'palette', pull: 'clone', put: false },
          sort: false,
          animation: 120,
          ghostClass: 'sortable-ghost',
          chosenClass: 'sortable-chosen'
        });
      }
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
    // Клик — доступная альтернатива перетаскиванию.
    card.addEventListener('click', function () {
      insertItem(selectedGroup, null, defaultCond(f.id));
      recomputeSelected();
      renderTree();
      scheduleValidate(0);
    });
    return card;
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
    clear(box);
    box.appendChild(renderGroup(model.root, '/root', 0));

    // Drop zone под условиями: принимает ингредиенты и условия.
    var target = groupAt(selectedGroup) || model.root;
    var dz = el('ul', { class: 'tree dropzone' });
    dz.setAttribute('data-list-path', selectedGroup);
    dz.setAttribute('data-append', '1');
    dz.setAttribute(
      'data-hint',
      'Перетащите ингредиент сюда — группа «' + (target.kind === 'any' ? 'ЛЮБОЕ' : 'ВСЕ') + '»'
    );
    box.appendChild(dz);

    bindSortables(box);
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
          removeItemAt(path); selectedGroup = '/root';
          renderTree(); scheduleValidate(0);
        }
      }));
    }

    head.appendChild(el('span', { class: 'spacer' }));
    head.appendChild(el('button', {
      class: 'small ghost quiet', type: 'button', text: 'Выбрать',
      title: 'Новые ингредиенты будут добавлены сюда',
      onclick: function () {
        selectedGroup = path;
        toast('ok', 'Группа выбрана: новые условия добавляются в неё.');
        renderTree();
      }
    }));
    head.appendChild(el('button', {
      class: 'small add', type: 'button', text: '+ группа',
      disabled: editable ? null : 'disabled',
      onclick: function () {
        group.items.push({ type: 'group', kind: 'any', items: [] });
        selectedGroup = path + '/items/' + (group.items.length - 1);
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

    if (selectedGroup === path) {
      head.appendChild(el('span', { class: 'badge public', text: 'приёмник' }));
    }
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

    group.items.forEach(function (item, i) {
      var p = path + '/items/' + i;
      var li = el('li', { class: 'node' });
      li.setAttribute('data-path', p);
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
        if (nf.operators.indexOf(item.op) < 0) item.op = nf.operators[0];
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
    f.operators.forEach(function (oid) {
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
    if (item.op === 'between') {
      if (!Array.isArray(item.value) || item.value.length !== 2) item.value = [0, 100];
      var wrap = el('span', { class: 'type-label', text: 'между' });
      var lo = el('input', {
        type: 'number', value: String(item.value[0]), style: 'width:8ch',
        'aria-label': 'Нижняя граница',
        disabled: editable ? null : 'disabled',
        oninput: function (e) { item.value[0] = parseInt(e.target.value, 10) || 0; scheduleValidate(); }
      });
      var hi = el('input', {
        type: 'number', value: String(item.value[1]), style: 'width:8ch',
        'aria-label': 'Верхняя граница',
        disabled: editable ? null : 'disabled',
        oninput: function (e) { item.value[1] = parseInt(e.target.value, 10) || 0; scheduleValidate(); }
      });
      wrap.appendChild(lo);
      wrap.appendChild(document.createTextNode(' и '));
      wrap.appendChild(hi);
      return wrap;
    }

    if (item.op === 'inTheLast' || item.op === 'notInTheLast') {
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

    if (field.valueType === 'bool') {
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

    if (field.valueType === 'number') {
      if (typeof item.value !== 'number') item.value = 0;
      return el('input', {
        type: 'number', value: String(item.value), style: 'width:12ch',
        'aria-label': 'Числовое значение',
        disabled: editable ? null : 'disabled',
        oninput: function (e) { item.value = parseInt(e.target.value, 10) || 0; scheduleValidate(); }
      });
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

  function bindSortables(container) {
    if (!window.Sortable) return;
    container.querySelectorAll('ul.tree[data-list-path]').forEach(function (ul) {
      var opts = {
        group: { name: 'tree', put: ['tree', 'palette'] },
        animation: 120,
        ghostClass: 'sortable-ghost',
        chosenClass: 'sortable-chosen',
        disabled: !editable,
        onEnd: function (evt) {
          // Drop zone принимает в конец списка.
          var append = evt.to.getAttribute('data-append') === '1';
          var chipField = evt.item.getAttribute('data-chip-field');
          if (chipField) {
            // Перетаскивание ингредиента из палитры (клон).
            evt.item.parentNode && evt.item.parentNode.removeChild(evt.item);
            insertItem(ul.getAttribute('data-list-path'), append ? null : evt.newIndex, defaultCond(chipField));
            recomputeSelected();
            renderAll(); scheduleValidate(0);
            return;
          }
          var itemPath = evt.item.getAttribute('data-path');
          if (!itemPath) { renderAll(); return; }
          var target = evt.to.getAttribute('data-list-path');
          var moved = removeItemAt(itemPath);
          if (!moved) { renderAll(); return; }
          insertItem(target, append ? null : evt.newIndex, moved);
          recomputeSelected();
          renderAll();
          scheduleValidate(0);
        }
      };
      // Перетаскивание вручную — за drag handle слева.
      if (!ul.classList.contains('dropzone')) opts.handle = '.drag-handle';
      window.Sortable.create(ul, opts);
    });
  }

  /* ------------------------------------------------------------------ */
  /* Валидация и предпросмотр                                            */
  /* ------------------------------------------------------------------ */

  function scheduleValidate(delay) {
    if (!schema) return;
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

  function runValidate() {
    if (!model) return;
    setValidity('pending', 'Проверяем…');
    api('/api/validate', { method: 'POST', body: model }).then(function (res) {
      var errs = errorsOf(res.data);
      lastValidateOk = res.ok && !errs.length;
      renderErrors(els['e-errors'], errs, lastValidateOk ? '' : 'Подборка не прошла валидацию');
      if (lastValidateOk && res.data.mix) {
        els['e-preview'].textContent = res.data.mix;
        els['e-preview'].parentNode.hidden = false;
      } else if (!lastValidateOk) {
        els['e-preview'].textContent = '';
      }
      setValidity(
        lastValidateOk ? 'ok' : 'err',
        lastValidateOk
          ? 'Проверено — можно публиковать'
          : 'Ошибки: ' + errs.length
      );
    }).catch(function () {
      renderErrors(els['e-errors'],
        [{ message: 'Сервер недоступен — проверка не выполнена.' }], 'Ошибка');
      lastValidateOk = false;
      setValidity('err', 'Сервер недоступен');
    });
  }

  /* ------------------------------------------------------------------ */
  /* Публикация                                                          */
  /* ------------------------------------------------------------------ */

  function publish() {
    runValidate();
    if (!lastValidateOk) {
      toast('error', 'Сначала исправьте ошибки валидации.');
      return;
    }
    sendPublish(false);
  }

  function sendPublish(overwrite) {
    var url = slug
      ? '/api/playlists/' + encodeURIComponent(slug) + (overwrite ? '?overwrite=1' : '')
      : '/api/playlists' + (overwrite ? '?overwrite=1' : '');
    api(url, { method: slug ? 'PUT' : 'POST', body: model }).then(function (res) {
      if (res.ok) {
        toast('ok', 'Подборка опубликована: ' + (res.data.entry ? res.data.entry.name : ''));
        var newSlug = res.data.entry ? res.data.entry.slug : slug;
        if (!slug && newSlug) {
          slug = newSlug;
          window.history.replaceState({}, '', '/edit/' + encodeURIComponent(slug));
        }
        if (res.data.mix) els['e-mix'].value = res.data.mix;
        if (res.data.nsp) els['e-nsp'].value = res.data.nsp;
        renderErrors(els['e-errors'], [], '');
        updatePath();
        return;
      }
      if (res.status === 409) {
        var e = errorsOf(res.data)[0] || {};
        if (e.code === 'external_readonly') {
          renderErrors(els['e-errors'], [e], 'Внешняя подборка');
          toast('error', 'Внешнюю подборку нельзя редактировать.');
          return;
        }
        confirmOverwrite(e.message || 'Файлы уже существуют.');
        return;
      }
      renderErrors(els['e-errors'], errorsOf(res.data), 'Не удалось опубликовать');
      toast('error', 'Публикация не выполнена.');
    }).catch(function () {
      toast('error', 'Сервер недоступен.');
    });
  }

  function confirmOverwrite(message) {
    var dlg = document.getElementById('overwrite-dialog');
    if (!dlg) { toast('warn', message); return; }
    document.getElementById('overwrite-text').textContent = message;
    dlg.returnValue = '';
    if (typeof dlg.showModal === 'function') dlg.showModal();
    else if (!window.confirm(message)) return;
    dlg.addEventListener('close', function handler() {
      dlg.removeEventListener('close', handler);
      if (dlg.returnValue === 'yes') sendPublish(true);
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
