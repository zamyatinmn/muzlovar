/* Client-only presentation strings. DTO, schema IDs and generated DSL stay untouched. */
(function (root) {
  'use strict';
  var messages = {
    ru: {
      'nav.playlists':'Подборки','nav.new':'Новая подборка','nav.trash':'Корзина',
      'brand.name':'Музловар','brand.subtitle':'редактор умных подборок Navidrome','brand.home':'К списку подборок','nav.aria':'Основная навигация',
      'footer.note':'Muzlovar — редактор умных подборок Navidrome. Файлы .mix и .nsp хранятся на диске; база данных Navidrome не изменяется напрямую.',
      'list.heading':'Умные подборки','list.empty':'Подборок пока нет.','list.create':'Создать первую подборку →',
      'list.title':'Название','list.description':'Описание','list.file':'Файл','list.properties':'Свойства','list.sort':'Сортировка','list.tree':'Дерево условий','list.status':'Статус','list.modified':'Изменено','list.actions':'Действия',
      'list.public':'публичная','list.private':'личная','list.limit':'лимит {count}','list.missingMix':'нет .mix','list.unpublished':'не опубликована','list.emptyGroup':'(пусто)','list.externalNode':'[внешний узел]',
      'status.managed':'управляемая','status.external':'внешняя','status.draft':'черновик','status.broken':'ошибка',
      'action.open':'Открыть','action.delete':'Удалить','action.cancel':'Отмена','action.overwrite':'Перезаписать','action.copy':'Копировать','action.copied':'Скопировано ✓',
      'action.publish':'Опубликовать','action.saveChanges':'Сохранить изменения','action.saveNew':'Сохранить как новую','action.saveNewTitle':'Сохранить рецепт как новую подборку',
      'dialog.deleteTitle':'Удалить подборку','dialog.deleteWarning':'Подборка помечена внешней: она создана не редактором. Файлы уйдут в корзину, но сущность в Navidrome останется.',
      'dialog.deletePrompt':'Подборка будет перенесена в корзину (восстановимо). Введите точное название:','dialog.exactName':'Точное название подборки',
      'dialog.overwriteTitle':'Файлы уже существуют','dialog.confirmDelete':'Удалить подборку «{name}»?',
      'editor.ingredients':'Ингредиенты','editor.search':'Поиск ингредиента…','editor.searchAria':'Поиск ингредиента','editor.recipe':'Рецепт подборки',
      'editor.name':'Название','editor.namePlaceholder':'Название подборки','editor.description':'Описание','editor.descriptionPlaceholder':'Зачем эта подборка',
      'editor.sort':'Порядок','editor.limit':'Лимит','editor.noLimitPlaceholder':'без лимита','editor.limitAria':'Лимит треков',
      'editor.public':'Публичная','editor.publicTitle':'Видна всем пользователям','editor.preview':'Предпросмотр','editor.validation':'Проверка',
      'editor.pending':'Ожидание…','editor.rules':'Правила','editor.compiledMix':'Скомпилированный .mix','editor.compiledNsp':'Скомпилированный .nsp (предпросмотр)','editor.dslLanguage':'Язык DSL',
      'editor.publishPath':'Путь публикации','editor.pathNote':'Только для информации: имя файла задаётся названием подборки, каталог — конфигурацией сервера (каталог .nsp Navidrome).',
      'editor.mixCode':'Код .mix','editor.mixDisk':'Код .mix на диске','editor.nspDisk':'.nsp на диске','editor.nspCode':'Код .nsp на диске',
      'editor.untitled':'Без названия','editor.noLimit':'Лимит не задан','editor.trackCount':'{count} {unit}',
      'editor.deleteTitle':'Перенести подборку в корзину','editor.deleteProduction':'Удалить из прода',
      'trash.back':'← К списку','trash.empty':'Корзина пуста. Удалённые подборки хранятся здесь до очистки.',
      'trash.id':'Идентификатор','trash.playlist':'Подборка','trash.deleted':'Удалена','trash.files':'Файлы','trash.restore':'Восстановить','trash.purge':'Удалить навсегда',
      'error.title':'Ошибка','error.back':'Вернуться к списку','error.notFound':'Страница не найдена.',
      'tree.groupLogic':'Логика группы','tree.dragGroup':'Перетащить группу','tree.drag':'Перетащить','tree.items':'{count} элем.',
      'tree.removeGroup':'Убрать группу','tree.selected':'✓ выбрана','tree.select':'Выбрать','tree.selectHint':'Клик по ингредиенту добавит условие в конец этой группы',
      'tree.selectedToast':'Группа выбрана: клик по ингредиенту добавит условие сюда.','tree.addGroup':'+ группа','tree.addCondition':'+ условие',
      'tree.externalNode':'внешний узел','tree.externalReadonly':'Не выражимо в DSL — доступно только чтение.','tree.removeUnknown':'Удалить неизвестный узел',
      'tree.field':'Поле','tree.operator':'Оператор','tree.noValue':'(без значения)','tree.moveUp':'Переместить вверх','tree.moveDown':'Переместить вниз','tree.removeCondition':'Удалить условие',
      'tree.between':'между','tree.and':' и ','tree.lower':'Нижняя граница','tree.upper':'Верхняя граница','tree.days':'за N дней:',
      'tree.daysAria':'Количество дней','tree.refKind':'Вид ссылки','tree.refValue':'Значение ссылки','tree.value':'Значение','tree.number':'Числовое значение','tree.text':'Текстовое значение',
      'tree.any':'ЛЮБОЕ','tree.all':'ВСЕ','tree.empty':'нет условий','tree.externalView':'внешний узел — только чтение','tree.personal':'личное','tree.personalTitle':'Персональное поле',
      'sort.mode':'Режим сортировки','sort.random':'случайно','sort.fields':'по полям','sort.default':'по умолчанию',
      'sort.external':'Сортировка из внешнего файла не поддерживается редактором: {text}','sort.field':'Поле сортировки','sort.direction':'Направление','sort.remove':'Убрать поле сортировки','sort.add':'+ поле',
      'validation.errors':'Ошибки','validation.warnings':'Предупреждения','validation.failed':'Подборка не прошла валидацию','validation.checking':'Проверяем…',
      'validation.warnCount':'Проверено — есть предупреждения: {count}','validation.ok':'Проверено — можно публиковать','validation.errorCount':'Ошибки: {count}',
      'validation.serverUnavailable':'Сервер недоступен — проверка не выполнена.','validation.serverShort':'Сервер недоступен',
      'validation.personal':'Персональные поля ({fields}) зависят от пользователя и времени прослушивания — одно правило даёт разный плейлист.',
      'message.nothingToCopy':'Нечего копировать.','message.loadSchema':'Не удалось загрузить схему полей.',
      'message.externalReadonly':'Внешняя подборка: дерево содержит конструкции, неизвестные редактору. Редактирование отключено.',
      'message.http':'Ошибка HTTP {status}','message.fixValidation':'Сначала исправьте ошибки валидации.',
      'message.savedNew':'Сохранено как новая подборка: {name}','message.published':'Подборка опубликована: {name}',
      'message.externalPlaylist':'Внешняя подборка','message.externalEdit':'Внешнюю подборку нельзя редактировать.',
      'message.filesExist':'Файлы уже существуют.','message.publishFailed':'Не удалось опубликовать','message.publishNotDone':'Публикация не выполнена.',
      'message.serverUnavailable':'Сервер недоступен.','message.deleteFailed':'Не удалось удалить','message.movedToTrash':'Перенесено в корзину.',
      'message.navidromeDeleted':' Сущность удалена в Navidrome.','message.navidromeRemains':' Сущность в Navidrome осталась: {reason}',
      'message.navidromeManual':' Сущность в Navidrome нужно удалить вручную (Subsonic не настроен).',
      'message.restored':'Подборка восстановлена.','message.purgeConfirm':'Удалить запись «{id}» из корзины безвозвратно?','message.purged':'Запись удалена.',
      'message.publishedPath':'Опубликовано:\n{path}\n\nБудет опубликовано:\n{next}',
      'error.line':'строка {line}','error.column':'столбец {column}',
      'api.nameRequired':'Название подборки не может быть пустым.','api.groupRequired':'Группа условий не может быть пустой.',
      'api.sortRequired':'Список полей сортировки не может быть пустым.','api.daysPositive':'число дней должно быть положительным',
      'api.daysExpected':'ожидается число дней','api.dateExpected':'ожидается дата в формате ГГГГ-ММ-ДД',
      'api.valueUnsupported':'неподдерживаемое значение условия','api.externalNode':'Элемент из внешнего файла не может быть отредактирован.',
      'value.yes':'да','value.no':'нет','value.days':'{count} дн.',
      'value.day.one':'день','value.day.few':'дня','value.day.many':'дней',
      'schema.group.logic':'Логика','schema.group.history':'История','schema.group.meta':'Метаданные','schema.group.audio':'Аудио',
      'schema.group.files':'Файлы','schema.group.album':'Альбом','schema.group.artist':'Артист','schema.group.ids':'Идентификаторы','schema.group.links':'Ссылки',
      'schema.ref.id':'ID','schema.ref.path':'путь к файлу','schema.enum.unknown':'Не определено'
    },
    en: {
      'nav.playlists':'Playlists','nav.new':'New playlist','nav.trash':'Trash',
      'brand.name':'Muzlovar','brand.subtitle':'Navidrome smart playlist editor','brand.home':'Back to playlists','nav.aria':'Main navigation',
      'footer.note':'Muzlovar is a Navidrome smart playlist editor. .mix and .nsp files are stored on disk; the Navidrome database is not modified directly.',
      'list.heading':'Smart playlists','list.empty':'No playlists yet.','list.create':'Create your first playlist →',
      'list.title':'Name','list.description':'Description','list.file':'File','list.properties':'Properties','list.sort':'Sort','list.tree':'Condition tree','list.status':'Status','list.modified':'Modified','list.actions':'Actions',
      'list.public':'Public','list.private':'Private','list.limit':'Limit {count}','list.missingMix':'Missing .mix','list.unpublished':'Unpublished','list.emptyGroup':'(empty)','list.externalNode':'[external node]',
      'status.managed':'Managed','status.external':'External','status.draft':'Draft','status.broken':'Error',
      'action.open':'Open','action.delete':'Delete','action.cancel':'Cancel','action.overwrite':'Overwrite','action.copy':'Copy','action.copied':'Copied ✓',
      'action.publish':'Publish','action.saveChanges':'Save changes','action.saveNew':'Save as new','action.saveNewTitle':'Save this recipe as a new playlist',
      'dialog.deleteTitle':'Delete playlist','dialog.deleteWarning':'This is an external playlist created outside the editor. Its files will move to Trash, but its Navidrome entry will remain.',
      'dialog.deletePrompt':'The playlist will move to Trash and can be restored. Enter its exact name to confirm:','dialog.exactName':'Exact playlist name',
      'dialog.overwriteTitle':'Files already exist','dialog.confirmDelete':'Delete playlist “{name}”?',
      'editor.ingredients':'Ingredients','editor.search':'Search ingredients…','editor.searchAria':'Search ingredients','editor.recipe':'Playlist recipe',
      'editor.name':'Name','editor.namePlaceholder':'Playlist name','editor.description':'Description','editor.descriptionPlaceholder':'What is this playlist for?',
      'editor.sort':'Sort','editor.limit':'Limit','editor.noLimitPlaceholder':'No limit','editor.limitAria':'Track limit',
      'editor.public':'Public','editor.publicTitle':'Visible to all users','editor.preview':'Preview','editor.validation':'Validation',
      'editor.pending':'Waiting…','editor.rules':'Rules','editor.compiledMix':'Compiled .mix','editor.compiledNsp':'Compiled .nsp (preview)','editor.dslLanguage':'DSL language',
      'editor.publishPath':'Publish path','editor.pathNote':'For reference: the playlist name determines the filename; the server configuration determines the directory (Navidrome .nsp directory).',
      'editor.mixCode':'.mix code','editor.mixDisk':'.mix code on disk','editor.nspDisk':'.nsp on disk','editor.nspCode':'.nsp code on disk',
      'editor.untitled':'Untitled','editor.noLimit':'No limit set','editor.trackCount':'{count} {unit}',
      'editor.deleteTitle':'Move playlist to Trash','editor.deleteProduction':'Move to Trash',
      'trash.back':'← Back to playlists','trash.empty':'Trash is empty. Deleted playlists remain here until permanently removed.',
      'trash.id':'ID','trash.playlist':'Playlist','trash.deleted':'Deleted','trash.files':'Files','trash.restore':'Restore','trash.purge':'Delete permanently',
      'error.title':'Error','error.back':'Back to playlists','error.notFound':'Page not found.',
      'tree.groupLogic':'Group logic','tree.dragGroup':'Drag group','tree.drag':'Drag','tree.items':'{count} items',
      'tree.removeGroup':'Remove group','tree.selected':'✓ selected','tree.select':'Select','tree.selectHint':'Click an ingredient to add its condition to the end of this group',
      'tree.selectedToast':'Group selected. Click an ingredient to add a condition here.','tree.addGroup':'+ group','tree.addCondition':'+ condition',
      'tree.externalNode':'external node','tree.externalReadonly':'Cannot be represented in the DSL; read-only.','tree.removeUnknown':'Remove unknown node',
      'tree.field':'Field','tree.operator':'Operator','tree.noValue':'(no value)','tree.moveUp':'Move up','tree.moveDown':'Move down','tree.removeCondition':'Remove condition',
      'tree.between':'between','tree.and':' and ','tree.lower':'Lower bound','tree.upper':'Upper bound','tree.days':'within N days:',
      'tree.daysAria':'Number of days','tree.refKind':'Reference type','tree.refValue':'Reference value','tree.value':'Value','tree.number':'Numeric value','tree.text':'Text value',
      'tree.any':'ANY','tree.all':'ALL','tree.empty':'no conditions','tree.externalView':'external node — read-only','tree.personal':'personal','tree.personalTitle':'User-specific field',
      'sort.mode':'Sort mode','sort.random':'Random','sort.fields':'Custom','sort.default':'Default',
      'sort.external':'Sorting from an external file is not supported by the editor: {text}','sort.field':'Sort field','sort.direction':'Direction','sort.remove':'Remove sort field','sort.add':'+ field',
      'validation.errors':'Errors','validation.warnings':'Warnings','validation.failed':'Playlist validation failed','validation.checking':'Checking…',
      'validation.warnCount':'Validated with {count} warnings','validation.ok':'Validated — ready to publish','validation.errorCount':'Errors: {count}',
      'validation.serverUnavailable':'Server unavailable — validation could not run.','validation.serverShort':'Server unavailable',
      'validation.personal':'User-specific fields ({fields}) depend on the user and listening history. The same rule can produce different playlists.',
      'message.nothingToCopy':'Nothing to copy.','message.loadSchema':'Could not load the field schema.',
      'message.externalReadonly':'External playlist: its condition tree contains constructs this editor does not recognize. Editing is disabled.',
      'message.http':'HTTP error {status}','message.fixValidation':'Fix validation errors first.',
      'message.savedNew':'Saved as a new playlist: {name}','message.published':'Playlist published: {name}',
      'message.externalPlaylist':'External playlist','message.externalEdit':'External playlists cannot be edited.',
      'message.filesExist':'Files already exist.','message.publishFailed':'Could not publish','message.publishNotDone':'Could not publish.',
      'message.serverUnavailable':'Server unavailable.','message.deleteFailed':'Could not delete','message.movedToTrash':'Moved to Trash.',
      'message.navidromeDeleted':' Navidrome entry deleted.','message.navidromeRemains':' Navidrome entry remains: {reason}',
      'message.navidromeManual':' Remove the Navidrome entry manually (Subsonic is not configured).',
      'message.restored':'Playlist restored.','message.purgeConfirm':'Permanently delete “{id}” from Trash?','message.purged':'Entry deleted.',
      'message.publishedPath':'Published:\n{path}\n\nWill publish to:\n{next}',
      'error.line':'line {line}','error.column':'column {column}',
      'api.nameRequired':'Playlist name is required.','api.groupRequired':'A condition group cannot be empty.',
      'api.sortRequired':'The sort field list cannot be empty.','api.daysPositive':'The number of days must be positive.',
      'api.daysExpected':'Expected a number of days.','api.dateExpected':'Expected a date in YYYY-MM-DD format.',
      'api.valueUnsupported':'Unsupported condition value.','api.externalNode':'An external file node cannot be edited.',
      'value.yes':'yes','value.no':'no','value.days':'{count} d',
      'value.day.one':'day','value.day.few':'days','value.day.many':'days',
      'schema.group.logic':'Logic','schema.group.history':'History','schema.group.meta':'Metadata','schema.group.audio':'Audio',
      'schema.group.files':'Files','schema.group.album':'Album','schema.group.artist':'Artist','schema.group.ids':'Identifiers','schema.group.links':'Links',
      'schema.ref.id':'ID','schema.ref.path':'File path','schema.enum.unknown':'Unknown'
    }
  };
  var fieldNames = {
    loved:'Loved',rating:'Rating',averagerating:'Average rating',hascoverart:'Has cover art',compilation:'Compilation',
    playcount:'Play count',lastplayed:'Last played',dateadded:'Date added',dateloved:'Date track was loved',daterated:'Date rated',
    title:'Title',album:'Album',genre:'Genre',year:'Year',explicitstatus:'Explicit',date:'Recording date',originalyear:'Original year',
    originaldate:'Original date',releaseyear:'Release year',releasedate:'Release date',tracknumber:'Track number',discnumber:'Disc number',
    discsubtitle:'Disc subtitle',comment:'Comment',lyrics:'Lyrics',sorttitle:'Sort title',sortalbum:'Sort album',sortartist:'Sort artist',
    sortalbumartist:'Sort album artist',catalognumber:'Catalog number',rgtrackgain:'ReplayGain track gain',rgtrackpeak:'ReplayGain track peak',
    rgalbumgain:'ReplayGain album gain',rgalbumpeak:'ReplayGain album peak',duration:'Duration',codec:'Codec',bitrate:'Bitrate',
    bitdepth:'Bit depth',samplerate:'Sample rate',bpm:'BPM',channels:'Channels',filepath:'File path',filetype:'File type',size:'Size',
    datemodified:'Date modified',missing:'File missing',albumcomment:'Album comment',albumrating:'Album rating',albumloved:'Album starred',
    albumplaycount:'Album play count',albumlastplayed:'Album last played',albumdateloved:'Date album was starred',albumdaterated:'Album date rated',
    albumdateadded:'Album date added',albumdatemodified:'Album date modified',albumduration:'Album duration',albumsongcount:'Album track count',
    albumsize:'Album size',artistrating:'Artist rating',artistloved:'Artist starred',artistplaycount:'Artist play count',
    artistlastplayed:'Artist last played',artistdateloved:'Date artist was starred',artistdaterated:'Artist date rated',
    mbz_album_id:'MusicBrainz album ID',mbz_album_artist_id:'MusicBrainz album artist ID',mbz_artist_id:'MusicBrainz artist ID',
    mbz_recording_id:'MusicBrainz recording ID',mbz_release_track_id:'MusicBrainz release track ID',
    mbz_release_group_id:'MusicBrainz release group ID',library_id:'Library',inPlaylist:'Playlist'
  };
  var operators = {
    eq:['=','equals'],ne:['!=','does not equal'],gt:['>','greater than'],ge:['>=','at least'],lt:['<','less than'],le:['<=','at most'],
    contains:['contains','contains text'],notContains:['does not contain','does not contain text'],startsWith:['starts with','starts with text'],
    endsWith:['ends with','ends with text'],between:['between','inclusive range'],inTheLast:['within N days','date is within the last N days'],
    notInTheLast:['not within N days','date is not within the last N days'],before:['before','earlier than date'],after:['after','later than date'],
    isMissing:['is missing','field is empty'],isPresent:['is present','field has a value'],bare:['flag','condition without a value'],
    inPlaylist:['in playlist','track is in the referenced playlist'],notInPlaylist:['not in playlist','track is not in the referenced playlist']
  };
  var staticTargets = [
    ['.brand','brand.home','title'],['.brand .logo','brand.name'],['.brand .subtitle','brand.subtitle'],['.app-nav','nav.aria','aria-label'],
    ['.app-nav a:nth-child(1)','nav.playlists'],['.app-nav a:nth-child(2)','nav.new'],['.app-nav a:nth-child(3)','nav.trash'],
    ['footer.app','footer.note'],['body:not(:has(.toolbar a.btn)) .toolbar h2','list.heading'],['.state.empty p:first-child','list.empty'],['.state.empty a','list.create'],
    ['table.list thead th:nth-child(1)','list.title'],['table.list thead th:nth-child(2)','list.description'],
    ['table.list thead th:nth-child(3)','list.file'],['table.list thead th:nth-child(4)','list.properties'],
    ['table.list thead th:nth-child(5)','list.sort'],['table.list thead th:nth-child(6)','list.tree'],
    ['table.list thead th:nth-child(7)','list.status'],['table.list thead th:nth-child(8)','list.modified'],
    ['table.list thead th:nth-child(9)','list.actions'],['table.list tbody a.btn','action.open'],
    ['table.list tbody button[data-delete]','action.delete'],['.badge.public','list.public'],['.badge.private','list.private'],['.badge.stale','list.missingMix'],
    ['table.list td:nth-child(4) .badge.draft','list.unpublished'],['table.list td:nth-child(7) .badge.draft','status.draft'],
    ['table.list td:nth-child(7) .badge.managed','status.managed'],['table.list td:nth-child(7) .badge.external','status.external'],
    ['table.list td:nth-child(7) .badge.broken:first-child','status.broken'],['#delete-dialog h3','dialog.deleteTitle'],['#delete-warning','dialog.deleteWarning'],
    ['#delete-dialog p:not(#delete-warning)','dialog.deletePrompt'],['#delete-input','dialog.exactName','aria-label'],
    ['#delete-dialog .row button:first-child','action.cancel'],['#delete-confirm','action.delete'],
    ['#overwrite-dialog h3','dialog.overwriteTitle'],['#overwrite-dialog .row button:first-child','action.cancel'],
    ['#overwrite-dialog .row button:last-child','action.overwrite'],
    ['body:has(#editor[data-published="0"]) #e-publish','action.publish'],
    ['body:has(#editor[data-published="1"]) #e-publish','action.saveChanges'],
    ['#e-publish-new','action.saveNew'],['#e-publish-new','action.saveNewTitle','title'],
    ['.col-left .col-head h2','editor.ingredients'],['#e-search','editor.search','placeholder'],['#e-search','editor.searchAria','aria-label'],
    ['.col-center .col-head h2','editor.recipe'],['label[for="e-name"]','editor.name'],['#e-name','editor.namePlaceholder','placeholder'],
    ['label[for="e-desc"]','editor.description'],['#e-desc','editor.descriptionPlaceholder','placeholder'],
    ['.result-row .foot-field:nth-child(1) .foot-label','editor.sort'],['.result-row .foot-field:nth-child(2) .foot-label','editor.limit'],
    ['#e-limit','editor.noLimitPlaceholder','placeholder'],['#e-limit','editor.limitAria','aria-label'],
    ['label[for="e-public"]','editor.public'],['label[for="e-public"]','editor.publicTitle','title'],
    ['.col-right .col-head h2','editor.preview'],['.col-right .block:first-of-type .block-title','editor.validation'],
    ['#e-validity-text','editor.pending'],['.tab[data-tab="rules"]','editor.rules'],
    ['#tab-mix .block-title','editor.compiledMix'],['#tab-nsp .block-title','editor.compiledNsp'],
    ['details:has(#e-mix) .block-summary .block-title','editor.mixCode'],['details:has(#e-mix) .block-label','editor.mixDisk'],
    ['#e-mix','editor.mixDisk','aria-label'],['details:has(#e-nsp) .block-summary .block-title','editor.nspDisk'],
    ['details:has(#e-nsp) .block-label','editor.nspCode'],['#e-nsp','editor.nspCode','aria-label'],
    ['#e-copy-preview','action.copy'],['#e-copy-nsp','action.copy'],['#e-copy-mix','action.copy'],
    ['#e-path + .block-note','editor.pathNote'],['#e-path','editor.publishPath','data-heading'],
    ['#pl-title','editor.untitled'],['#pl-meta','editor.noLimit'],
    ['#e-delete','editor.deleteProduction'],['#e-delete','editor.deleteTitle','title'],
    ['.toolbar a.btn','trash.back'],['.state.empty:not(:has(p))','trash.empty'],
    ['table.list tbody button[data-restore]','trash.restore'],['table.list tbody button[data-purge]','trash.purge'],
    ['.state.error h2','error.title'],['.state.error a','error.back']
  ];
  function normalize(locale) { return typeof locale === 'string' && locale.toLowerCase().indexOf('ru') === 0 ? 'ru' : 'en'; }
  function storedLocale(storage, browserLanguage) {
    var saved;
    try { saved = storage.getItem('muzlovar.locale'); } catch (_) { saved = null; }
    return saved === 'ru' || saved === 'en' ? saved : normalize(browserLanguage);
  }
  var locale = storedLocale(root.localStorage || { getItem: function () { return null; } }, root.navigator && root.navigator.language);
  function t(key, params) {
    var template = messages[locale][key] || messages.ru[key] || key;
    return template.replace(/\{([a-zA-Z]+)\}/g, function (_, name) {
      return params && Object.prototype.hasOwnProperty.call(params, name) ? String(params[name]) : '{' + name + '}';
    });
  }
  function schemaText(kind, id, fallback, hint) {
    if (locale !== 'en') return fallback || id;
    if (kind === 'field') return fieldNames[id] || fallback || id;
    if (kind === 'operator') return operators[id] ? operators[id][hint ? 1 : 0] : fallback || id;
    if (kind === 'group') return t('schema.group.' + id) === 'schema.group.' + id ? fallback || id : t('schema.group.' + id);
    if (kind === 'logic') return t('tree.' + id);
    if (kind === 'direction') return id === 'asc' ? 'Ascending' : id === 'desc' ? 'Descending' : fallback || id;
    if (kind === 'ref') return t('schema.ref.' + id);
    if (kind === 'enum') return id === '' ? t('schema.enum.unknown') : fallback || id;
    return fallback || id;
  }
  var knownApiMessages = {
    'Название подборки не может быть пустым.':'api.nameRequired',
    'Группа условий не может быть пустой.':'api.groupRequired',
    'Список полей сортировки не может быть пустым.':'api.sortRequired',
    'число дней должно быть положительным':'api.daysPositive',
    'ожидается число дней':'api.daysExpected',
    'ожидается дата в формате ГГГГ-ММ-ДД':'api.dateExpected',
    'неподдерживаемое значение условия':'api.valueUnsupported',
    'Элемент из внешнего файла не может быть отредактирован.':'api.externalNode'
  };
  function apiMessage(error) {
    var raw = error && error.message || String(error);
    return knownApiMessages[raw] ? t(knownApiMessages[raw]) : raw;
  }
  function applyStatic() {
    if (!root.document) return;
    root.document.documentElement.lang = locale;
    staticTargets.forEach(function (target) {
      root.document.querySelectorAll(target[0]).forEach(function (node) {
        if (target[2]) node.setAttribute(target[2], t(target[1]));
        else node.textContent = t(target[1]);
      });
    });
    var path = root.document.getElementById('e-path');
    if (path) {
      var pathHeading = path.parentNode && path.parentNode.querySelector('.block-head .block-title');
      if (pathHeading) pathHeading.textContent = t('editor.publishPath');
    }
    root.document.querySelectorAll('.badge.limit').forEach(function (badge) {
      var count = badge.getAttribute('data-count');
      if (!count) {
        var match = badge.textContent.match(/\d+/);
        count = match ? match[0] : '';
        badge.setAttribute('data-count', count);
      }
      badge.textContent = t('list.limit', {count:count});
    });
    if (root.document.querySelector('.toolbar a.btn')) {
      var heading = root.document.querySelector('.toolbar h2');
      if (heading) heading.textContent = t('nav.trash');
      ['trash.id','trash.playlist','trash.deleted','trash.files','list.actions'].forEach(function (key, index) {
        var cell = root.document.querySelector('table.list thead th:nth-child(' + (index + 1) + ')');
        if (cell) cell.textContent = t(key);
      });
    }
    var errorParagraph = root.document.querySelector('.state.error > p:first-of-type');
    if (errorParagraph && (errorParagraph.textContent === messages.ru['error.notFound'] ||
        errorParagraph.textContent === messages.en['error.notFound'])) {
      errorParagraph.textContent = t('error.notFound');
    }
    var titleKey = root.document.documentElement.getAttribute('data-title-key');
    if (!titleKey && root.document.title.indexOf('Muzlovar — ') === 0) {
      var suffix = root.document.title.slice(11);
      var pages = {'подборки':'nav.playlists','новая подборка':'nav.new','корзина':'nav.trash','ошибка':'error.title'};
      titleKey = pages[suffix];
      if (titleKey) root.document.documentElement.setAttribute('data-title-key', titleKey);
    }
    if (titleKey) root.document.title = 'Muzlovar — ' + t(titleKey);
  }
  function setLocale(next) {
    locale = normalize(next);
    try { root.localStorage.setItem('muzlovar.locale', locale); } catch (_) { /* private mode */ }
    applyStatic();
    var switcher = root.document && root.document.getElementById('ui-locale');
    if (switcher) switcher.value = locale;
    if (root.document) root.document.dispatchEvent(new root.CustomEvent('muzlovar:localechange'));
    return locale;
  }
  function mountSwitcher() {
    if (!root.document) return;
    var host = root.document.querySelector('.header-actions');
    if (!host) return;
    var select = root.document.createElement('select');
    select.id = 'ui-locale'; select.className = 'locale-switch';
    select.setAttribute('aria-label', locale === 'ru' ? 'Язык интерфейса' : 'Interface language');
    [['ru','Русский'],['en','English']].forEach(function (pair) {
      var option = root.document.createElement('option'); option.value = pair[0]; option.textContent = pair[1]; select.appendChild(option);
    });
    select.value = locale;
    select.addEventListener('change', function () { setLocale(select.value); select.setAttribute('aria-label', locale === 'ru' ? 'Язык интерфейса' : 'Interface language'); });
    host.insertBefore(select, host.firstChild);
  }
  root.MuzlovarI18n = { t:t, setLocale:setLocale, getLocale:function () { return locale; }, storedLocale:storedLocale,
    normalize:normalize, schemaText:schemaText, apiMessage:apiMessage, applyStatic:applyStatic, mountSwitcher:mountSwitcher, messages:messages };
}(typeof window === 'undefined' ? globalThis : window));
