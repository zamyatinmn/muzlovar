/* Muzlovar — визуальный редактор умных подборок Navidrome.
 *
 * Два принципа:
 *  1. Списки полей и операторов приходят только с /api/schema —
 *     здесь нет собственных копий этих списков.
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
      'e-personal', 'e-save', 'e-publish', 'e-delete',
      'e-mix', 'e-nsp', 'toasts'
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
      bindForm();
      renderAll();
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
      model.name = els['e-name'].value; scheduleValidate();
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
      scheduleValidate();
    });

    // Персональные поля — предупреждение в UI.
    if (els['e-personal'] && schema.personalFields.length) {
      els['e-personal'].textContent =
        'Персональные поля (' + schema.personalFields.join(', ') + ') зависят от текущего пользователя ' +
        'и времени прослушивания: одинаковое правило у разных пользователей даёт разные плейлисты.';
      els['e-personal'].hidden = false;
    }

    if (els['e-publish']) els['e-publish'].addEventListener('click', publish);
    if (els['e-save']) els['e-save'].addEventListener('click', function () { scheduleValidate(0); });
    if (els['e-delete']) els['e-delete'].addEventListener('click', confirmDelete);
  }

  /* ------------------------------------------------------------------ */
  /* Палитра                                                             */
  /* ------------------------------------------------------------------ */

  function buildPalette() {
    var box = els['e-palette'];
    clear(box);
    schema.fields.forEach(function (f) {
      var chip = el('button', {
        class: 'chip',
        type: 'button',
        title: f.title + ' (' + f.nspName + ')',
        text: f.title
      });
      chip.setAttribute('data-chip-field', f.id);
      // Клик — доступная альтернатива перетаскиванию.
      chip.addEventListener('click', function () {
        insertItem(selectedGroup, null, defaultCond(f.id));
        recomputeSelected();
        renderTree();
        scheduleValidate(0);
      });
      box.appendChild(chip);
    });

    if (window.Sortable) {
      window.Sortable.create(box, {
        group: { name: 'palette', pull: 'clone', put: false },
        sort: false,
        animation: 120
      });
    }
  }

  /* ------------------------------------------------------------------ */
  /* Дерево                                                              */
  /* ------------------------------------------------------------------ */

  function renderAll() {
    els['e-name'].value = model.name || '';
    els['e-desc'].value = model.description || '';
    els['e-public'].checked = !!model.public;
    els['e-limit'].value = (model.limit === null || model.limit === undefined) ? '' : String(model.limit);
    renderTree();
    renderSort();
  }

  function renderTree() {
    var box = els['e-tree'];
    clear(box);
    var rootList = renderGroup(model.root, '/root', 0);
    box.appendChild(rootList);
    bindSortables(box);
  }

  function groupHead(group, path, depth) {
    var head = el('div', { class: 'group-head' });

    var sel = el('select', {
      'aria-label': 'Логика группы',
      onchange: function (e) { group.kind = e.target.value; scheduleValidate(); }
    });
    schema.groupKinds.forEach(function (k) {
      var o = el('option', { value: k.id, text: k.name });
      if (k.id === group.kind) o.selected = true;
      sel.appendChild(o);
    });
    head.appendChild(el('span', { class: 'kind', text: 'Группа' }));
    head.appendChild(sel);
    head.appendChild(el('span', { class: 'count', text: group.items.length + ' элем.' }));

    if (path !== '/root') {
      head.appendChild(el('button', {
        class: 'small ghost', type: 'button', text: 'Убрать группу',
        disabled: editable ? null : 'disabled',
        onclick: function () {
          removeItemAt(path); selectedGroup = '/root';
          renderTree(); scheduleValidate(0);
        }
      }));
    }

    head.appendChild(el('span', { class: 'spacer' }));
    head.appendChild(el('button', {
      class: 'small', type: 'button', text: 'Выбрать',
      title: 'Новые условия из палитры будут добавлены сюда',
      onclick: function () {
        selectedGroup = path;
        toast('ok', 'Группа выбрана: новые условия добавляются в неё.');
      }
    }));
    head.appendChild(el('button', {
      class: 'small', type: 'button', text: '+ группа',
      disabled: editable ? null : 'disabled',
      onclick: function () {
        group.items.push({ type: 'group', kind: 'any', items: [] });
        selectedGroup = path + '/items/' + (group.items.length - 1);
        renderTree(); scheduleValidate(0);
      }
    }));
    head.appendChild(el('button', {
      class: 'small', type: 'button', text: '+ условие',
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
    var wrap = el('div', { class: depth === 0 ? 'group' : 'node-group' });
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
        class: 'small danger', type: 'button', text: 'Удалить',
        title: 'Удалить неизвестный узел',
        onclick: function () { removeItemAt(path); renderTree(); scheduleValidate(0); }
      }));
    }
    return box;
  }

  function renderCond(item, path, index, group) {
    var box = el('div', { class: 'cond' });
    var f = fieldById(item.field) || schema.fields[0];

    box.appendChild(el('span', { class: 'type-label', text: 'поле' }));
    var fieldSel = el('select', {
      'aria-label': 'Поле',
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
      var o = el('option', { value: sf.id, text: sf.title });
      if (sf.id === item.field) o.selected = true;
      fieldSel.appendChild(o);
    });
    box.appendChild(fieldSel);

    if (isPersonal(f.id)) {
      box.appendChild(el('span', { class: 'badge draft', title: 'Персональное поле', text: 'личное' }));
    }

    box.appendChild(el('span', { class: 'type-label', text: 'оператор' }));
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
        text: op ? (op.name + ' — ' + op.hint) : oid
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
        class: 'small danger', type: 'button', text: '✕',
        'aria-label': 'Удалить условие',
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
  /* Сортировка и лимит                                                  */
  /* ------------------------------------------------------------------ */

  function renderSort() {
    var box = els['e-sort'];
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
          class: 'small danger', type: 'button', text: '✕',
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
        class: 'small', type: 'button', text: '+ поле',
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
      window.Sortable.create(ul, {
        group: { name: 'tree', put: ['tree', 'palette'] },
        animation: 120,
        ghostClass: 'sortable-ghost',
        chosenClass: 'sortable-chosen',
        disabled: !editable,
        onEnd: function (evt) {
          var chipField = evt.item.getAttribute('data-chip-field');
          if (chipField) {
            // Перетаскивание чипа из палитры (клон).
            evt.item.parentNode && evt.item.parentNode.removeChild(evt.item);
            insertItem(ul.getAttribute('data-list-path'), evt.newIndex, defaultCond(chipField));
            recomputeSelected();
            renderAll(); scheduleValidate(0);
            return;
          }
          var itemPath = evt.item.getAttribute('data-path');
          if (!itemPath) return;
          var target = evt.to.getAttribute('data-list-path');
          var moved = removeItemAt(itemPath);
          if (!moved) { renderAll(); return; }
          insertItem(target, evt.newIndex, moved);
          recomputeSelected();
          renderAll();
          scheduleValidate(0);
        }
      });
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

  function runValidate() {
    if (!model) return;
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
      if (els['e-save']) {
        els['e-save'].textContent = lastValidateOk
          ? (slug ? 'Проверено ✓' : 'Проверено ✓')
          : 'Есть ошибки';
      }
    }).catch(function () {
      renderErrors(els['e-errors'],
        [{ message: 'Сервер недоступен — проверка не выполнена.' }], 'Ошибка');
      lastValidateOk = false;
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
