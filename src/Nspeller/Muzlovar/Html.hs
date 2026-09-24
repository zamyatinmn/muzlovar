{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Серверные страницы Muzlovar (Lucid, тёмная тема).
--
-- Список подборок и корзина рендерятся на сервере; редактор отдаётся
-- каркасом, а дерево строит @muzlovar.js@ по данным @/api@. Русский
-- язык интерфейса и тёмная тема по умолчанию зафиксированы разметкой
-- и @muzlovar.css@.
module Nspeller.Muzlovar.Html
  ( renderHtml
  , layout
  , layoutWith
  , indexPage
  , editorPage
  , trashPage
  , errorPage
  , formatModified
  ) where

import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Time.Clock (UTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Lucid.Base (Html, Term (term), makeAttribute, renderText, toHtml)
import Lucid.Html5
import Nspeller.Muzlovar.Store (PlaylistEntry (..), TrashEntry (..))

-- | HTML-документ в тексте.
renderHtml :: Html () -> LT.Text
renderHtml = renderText

------------------------------------------------------------------------------
-- Общий каркас
------------------------------------------------------------------------------

-- | Страница с шапкой, подвалом, тостами и подключёнными скриптами.
layout :: Text -> Html () -> Html ()
layout title = layoutWith title Nothing

-- | Каркас страницы.
--
-- Второй аргумент — содержимое справа в шапке (кнопки редактора).
-- @Just@ переключает каркас в режим редактора: подвал скрыт, @main@
-- занимает всю высоту окна — три колонки на весь экран.
layoutWith :: Text -> Maybe (Html ()) -> Html () -> Html ()
layoutWith title mHeaderActions mainContent =
  doctypehtml_ (html_ [lang_ "ru"] page)
  where
    editorMode = isJust mHeaderActions

    page =
      head_
        ( meta_ [charset_ "utf-8"]
            <> meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
            <> meta_ [name_ "color-scheme", content_ "dark"]
            <> title_ (toHtml title)
            <> link_ [rel_ "icon", type_ "image/x-icon", href_ "/static/favicon.ico"]
            <> link_ [rel_ "stylesheet", href_ "/static/muzlovar.css"]
        )
        <> body_
          ( header_ [class_ "app"] (brand <> headerActions)
              <> main_ [class_ (if editorMode then "main-full" else "main")] mainContent
              <> footerEl
              <> div_ [id_ "toasts", makeAttribute "aria-live" "polite"] ""
              <> script_ [src_ "/static/muzlovar.js"] ("" :: Text)
              <> dialogs
          )

    -- Логотип — ссылка на список подборок (навигация есть и в меню настроек).
    brand =
      a_
        [href_ "/", class_ "brand", makeAttribute "title" "К списку подборок"]
        ( img_
            [ src_ "/static/logo.png"
            , alt_ ""
            , width_ "30"
            , height_ "30"
            , class_ "logo-img"
            ]
            -- Текст вынесен в общий baseline-контейнер: иконка центрируется
            -- по нему, а «Музловар» и подзаголовок остаются на одной линии.
            <> span_
              [class_ "brand-text"]
              ( span_ [class_ "logo"] "Музловар"
                  <> span_ [class_ "subtitle"] "редактор умных подборок Navidrome"
              )
        )

    headerActions =
      div_ [class_ "header-actions"] (connStatus <> settingsButton <> fromMaybe mempty mHeaderActions)

    -- Индикатор доступности прода: опрашивает публичный /health.
    connStatus =
      span_
        [ id_ "conn-status"
        , class_ "conn"
        , makeAttribute "title" "Состояние подключения к проду"
        ]
        ( span_ [class_ "conn-dot"] ""
            <> span_ [id_ "conn-text"] "Прод: проверка…"
        )

    settingsButton =
      button_
        [ id_ "settings-btn"
        , type_ "button"
        , class_ "icon-btn"
        , makeAttribute "title" "Настройки"
        , makeAttribute "aria-label" "Настройки"
        , makeAttribute "aria-haspopup" "dialog"
        ]
        "\x2699\xFE0E"

    footerEl =
      if editorMode
        then mempty
        else
          footer_
            [class_ "app"]
            "Muzlovar — редактор умных подборок Navidrome. Файлы .mix и .nsp \
            \хранятся на диске; база данных Navidrome не изменяется напрямую."

    dialogs = deleteDialog <> overwriteDialog <> settingsDialog

    settingsDialog =
      dialog_
        [id_ "settings-dialog"]
        ( h3_ "Настройки"
            <> nav_
              [class_ "settings-nav"]
              ( a_ [href_ "/"] "Подборки"
                  <> a_ [href_ "/new"] "Новая подборка"
                  <> a_ [href_ "/trash"] "Корзина"
              )
            <> p_
              [class_ "settings-note"]
              "Файлы .mix и .nsp хранятся на диске; база данных Navidrome \
              \не изменяется напрямую. Статус в шапке отражает доступность сервера."
            <> div_
              [class_ "row"]
              ( button_
                  [ type_ "button"
                  , makeAttribute "onclick" "document.getElementById('settings-dialog').close()"
                  ]
                  "Закрыть"
              )
        )

    deleteDialog =
      dialog_
        [id_ "delete-dialog"]
        ( h3_ "Удалить подборку"
            <> p_
              [ id_ "delete-warning"
              , class_ "state warn"
              , makeAttribute "hidden" "hidden"
              ]
              "Подборка помечена внешней: она создана не редактором. \
              \Файлы уйдут в корзину, но сущность в Navidrome останется."
            <> p_ "Подборка будет перенесена в корзину (восстановимо). Введите точное название:"
            <> code_ [id_ "delete-name", class_ "confirm-name"] ""
            <> input_
              [ id_ "delete-input"
              , type_ "text"
              , class_ "confirm-name"
              , autocomplete_ "off"
              , makeAttribute "aria-label" "Точное название подборки"
              ]
            <> div_
              [class_ "row"]
              ( button_
                  [ type_ "button"
                  , makeAttribute "onclick" "document.getElementById('delete-dialog').close()"
                  ]
                  "Отмена"
                  <> button_
                    [id_ "delete-confirm", type_ "button", class_ "danger", disabled_ ""]
                    "Удалить"
              )
        )

    overwriteDialog =
      dialog_
        [id_ "overwrite-dialog"]
        ( h3_ "Файлы уже существуют"
            <> p_ [id_ "overwrite-text"] ""
            <> div_
              [class_ "row"]
              ( button_
                  [ type_ "button"
                  , makeAttribute "onclick" "document.getElementById('overwrite-dialog').close()"
                  ]
                  "Отмена"
                  <> button_
                    [ type_ "button"
                    , class_ "primary"
                    , makeAttribute "onclick" "var d=document.getElementById('overwrite-dialog');d.returnValue='yes';d.close()"
                    ]
                    "Перезаписать"
              )
        )

-- | Разделитель-заполнитель в панелях инструментов.
spacer_ :: Html ()
spacer_ = span_ [class_ "spacer"] ""

-- | Элемент @dialog@ (в этой версии Lucid отсутствует).
dialog_ :: Term arg result => arg -> result
dialog_ = term "dialog"

-- | Элементы @details@/@summary@ для сворачиваемых блоков кода
-- (в этой версии Lucid отсутствуют).
detailsEl :: Term arg result => arg -> result
detailsEl = term "details"

summaryEl :: Term arg result => arg -> result
summaryEl = term "summary"

------------------------------------------------------------------------------
-- Список подборок
------------------------------------------------------------------------------

-- | Главная страница: таблица всех @.mix@ и @.nsp@.
indexPage :: [PlaylistEntry] -> Html ()
indexPage entries =
  layout
    "Muzlovar — подборки"
    ( div_
        [class_ "toolbar"]
        ( h2_ "Умные подборки"
            <> spacer_
            <> a_ [href_ "/trash", class_ "btn"] "Корзина"
            <> a_ [href_ "/new", class_ "btn primary"] "Новая подборка"
        )
        <> if null entries
          then
            div_
              [class_ "state empty"]
              ( p_ "Подборок пока нет."
                  <> p_
                    ( a_ [href_ "/new"]
                        "Создать первую подборку →"
                    )
              )
          else table_ [class_ "list"] (headerRow <> rows)
    )
  where
    headerRow =
      thead_
        ( tr_
            ( th_ "Название"
                <> th_ "Описание"
                <> th_ "Файл"
                <> th_ "Свойства"
                <> th_ "Сортировка"
                <> th_ "Дерево условий"
                <> th_ "Статус"
                <> th_ "Изменено"
                <> th_ "Действия"
            )
        )

    rows = tbody_ (foldMap entryRow entries)

    entryRow :: PlaylistEntry -> Html ()
    entryRow e =
      tr_
        ( td_ [class_ "title"] (toHtml (peTitle e))
            <> td_ [class_ "desc"] (toHtml (peDescription e))
            <> td_
              [class_ "file"]
              ( maybe mempty toHtml (peMixFile e)
                  <> br_ []
                  <> maybe mempty toHtml (peNspFile e)
              )
            <> td_ properties
            <> td_ (toHtml (peSort e))
            <> td_ [class_ "summary"] (toHtml (peSummary e))
            <> td_ status
            <> td_ (toHtml (formatModified (peModified e)))
            <> td_ [class_ "actions"] actions
        )
      where
        external = peStatus e == "external"

        properties =
          mconcat
            [ badge "public" (if pePublic e then "публичная" else "личная")
            , maybe mempty (\n -> badge "" ("лимит " <> T.pack (show n))) (peLimit e)
            , if peStale e then badge "stale" "нет .mix" else mempty
            , if peDraft e then badge "draft" "не опубликована" else mempty
            ]

        status =
          mconcat
            [ badge (peStatus e) (statusTitle (peStatus e))
            , maybe mempty (\err -> br_ [] <> span_ [class_ "badge broken"] (toHtml err)) (peError e)
            ]

        actions =
          mconcat
            [ a_ [href_ ("/edit/" <> peSlug e), class_ "btn"] "Открыть"
            , button_
                [ type_ "button"
                , class_ "danger small"
                , makeAttribute "data-delete" (peSlug e)
                , makeAttribute "data-delete-name" (peTitle e)
                , makeAttribute "data-external" (if external then "1" else "0")
                ]
                "Удалить"
            ]

    statusTitle = \case
      "managed" -> "управляемая"
      "external" -> "внешняя"
      "draft" -> "черновик"
      "broken" -> "ошибка"
      other -> other

badge :: Text -> Text -> Html ()
badge cls label = span_ ([class_ ("badge " <> cls)] <> [makeAttribute "hidden" "hidden" | T.null label]) (toHtml label)

------------------------------------------------------------------------------
-- Редактор
------------------------------------------------------------------------------

-- | Каркас редактора. @Nothing@ — новая подборка.
--
-- Второй аргумент — фактический путь опубликованного @.nsp@
-- ('Nothing', если подборка не опубликована), третий — настроенный
-- каталог публикации (.nsp): каталог отдаётся клиенту как
-- @data-publish-dir@, состояние — как @data-published@, путь файла —
-- как @data-published-path@. Блок «Путь публикации» до первой
-- публикации показывает каталог, после — полный путь файла; если
-- текущее название даст другой filename, muzlovar.js дописывает
-- блок «Будет опубликовано». Путь — только информация, без кнопок и
-- редактирования.
--
-- Раскладка desktop: три колонки на всю высоту окна — ингредиенты
-- (~22%), редактор подборки (~50%) и предпросмотр с публикацией
-- (~28%). Дерево условий, палитра и правая колонка строятся
-- @muzlovar.js@ по данным @/api@; здесь — каркас и статические
-- блоки. Правая колонка читается сверху вниз: карточка подборки и
-- статус проверки — главные, технический код .mix/.nsp — свёрнутые
-- блоки в самом низу.
editorPage :: Maybe Text -> Maybe FilePath -> FilePath -> Html ()
editorPage mslug publishedPath publishDir =
  layoutWith title (Just editorActions) workspace
  where
    title = case mslug of
      Just s -> "Muzlovar — " <> s
      Nothing -> "Muzlovar — новая подборка"

    -- Кнопки редактора живут в шапке (см. 'layoutWith').
    editorActions =
      button_
        [ id_ "e-save"
        , type_ "button"
        , class_ "btn"
        , makeAttribute "title" "Проверить подборку"
        ]
        "Проверить"
        <> button_ [id_ "e-publish", type_ "button", class_ "btn primary"] "Опубликовать"

    workspace =
      div_
        [ id_ "editor"
        , class_ "workspace"
        , makeAttribute "data-slug" (fromMaybe "" mslug)
        , makeAttribute "data-publish-dir" (T.pack publishDir)
        , makeAttribute
            "data-published"
            (case publishedPath of Just _ -> "1"; Nothing -> "0")
        , makeAttribute "data-published-path" (T.pack (fromMaybe "" publishedPath))
        ]
        (leftColumn <> centerColumn <> rightColumn)

    -- Начальное содержимое блока «Путь публикации»: после публикации —
    -- фактический путь файла на диске, до неё — только каталог
    -- публикации из конфигурации сервера. Дальше блоком управляет
    -- muzlovar.js (updatePath): при ожидающем переименовании добавляется
    -- вторая половина «Будет опубликовано».
    publishPathText :: Text
    publishPathText = maybe (T.pack publishDir) T.pack publishedPath

    -------------------------------------------------------------- Ингредиенты
    leftColumn =
      section_
        [class_ "col col-left"]
        ( colHead "Ингредиенты"
            <> div_
              [class_ "col-search"]
              ( input_
                  [ id_ "e-search"
                  , type_ "search"
                  , autocomplete_ "off"
                  , makeAttribute "placeholder" "Поиск ингредиента…"
                  , makeAttribute "aria-label" "Поиск ингредиента"
                  ]
              )
            -- Группы «Логика»/«История»/«Метаданные» и карточки
            -- строит muzlovar.js из /api/schema.
            <> div_ [id_ "e-palette", class_ "col-body"] ""
        )

    ---------------------------------------------------------- Рецепт подборки
    centerColumn =
      section_
        [class_ "col col-center"]
        ( colHead "Рецепт подборки"
            <> div_
              [class_ "col-body"]
              ( div_
                  [class_ "fields-row"]
                  ( textField "Название" "e-name" "Название подборки"
                      <> textField "Описание" "e-desc" "Зачем эта подборка"
                  )
                  -- Параметры результата — компактной строкой сразу после
                  -- полей и перед конструктором правил: внизу колонки
                  -- «Порядок», «Лимит» и «Публичная» терялись.
                  <> resultRow
                  <> p_ [id_ "e-personal", class_ "notice", makeAttribute "hidden" "hidden"] ""
                  <> div_ [id_ "e-tree", class_ "rules"] ""
              )
        )

    -------------------------------------------------------------- Предпросмотр
    rightColumn =
      section_
        [class_ "col col-right"]
        ( colHead "Предпросмотр"
            <> div_
              [class_ "col-body"]
              ( div_ [id_ "e-status", class_ "state loading", makeAttribute "hidden" "hidden"] ""
                  -- Карточка подборки и статус проверки — смысл колонки,
                  -- поэтому они идут сразу; технический код ниже.
                  <> playlistCard
                  <> section_
                    [class_ "block"]
                    ( span_ [class_ "block-title"] "Проверка"
                        <> div_ [id_ "e-validity", class_ "validity pending"]
                          (span_ [class_ "vdot"] "" <> span_ [id_ "e-validity-text"] "Ожидание…")
                        <> div_ [id_ "e-errors", makeAttribute "hidden" "hidden"] ""
                    )
                  <> div_ [class_ "tabs", makeAttribute "role" "tablist"]
                    ( tabButton "rules" "Правила" True
                        <> tabButton "mix" ".mix" False
                        <> tabButton "nsp" ".nsp" False
                    )
                  <> div_ [id_ "tab-rules", class_ "tab-panel"]
                    (div_ [id_ "e-rules", class_ "view-tree"] "")
                  <> div_ [id_ "tab-mix", class_ "tab-panel", makeAttribute "hidden" "hidden"]
                    ( div_
                        [id_ "e-preview-wrap", makeAttribute "hidden" "hidden"]
                        ( blockHead
                            "Скомпилированный .mix"
                            (button_ [id_ "e-copy-preview", type_ "button", class_ "btn small"] "Копировать")
                            <> pre_ [id_ "e-preview", class_ "code"] ""
                        )
                    )
                  <> div_ [id_ "tab-nsp", class_ "tab-panel", makeAttribute "hidden" "hidden"]
                    ( div_
                        [id_ "e-nsp-preview-wrap", makeAttribute "hidden" "hidden"]
                        ( blockHead
                            "Скомпилированный .nsp (предпросмотр)"
                            (button_ [id_ "e-copy-nsp", type_ "button", class_ "btn small"] "Копировать")
                            <> pre_ [id_ "e-preview-nsp", class_ "code"] ""
                        )
                    )
                  -- Путь публикации — только информация: кнопки
                  -- «Изменить» и редактирования нет. До первой
                  -- публикации показывается каталог из конфигурации
                  -- сервера, после — полный путь файла (см. updatePath
                  -- в muzlovar.js).
                  <> section_
                    [class_ "block"]
                    ( blockHead "Путь публикации" mempty
                        <> code_ [id_ "e-path", class_ "path"] (toHtml publishPathText)
                        <> p_
                          [class_ "block-note"]
                          "Только для информации: имя файла задаётся \
                          \названием подборки, каталог — конфигурацией \
                          \сервера (каталог .nsp Navidrome)."
                    )
                  -- Технический код на диске: свёрнут, пока не нужен.
                  <> collapsibleCode
                    "Код .mix"
                    "e-mix"
                    "5"
                    "Код .mix на диске"
                    (Just "e-copy-mix")
                  <> collapsibleCode ".nsp на диске" "e-nsp" "3" "Код .nsp на диске" Nothing
              )
            <> deleteFoot
        )

    -------------------------------------------------------------- Вспомогательное
    colHead :: Text -> Html ()
    colHead t = div_ [class_ "col-head"] (h2_ (toHtml t))

    textField :: Text -> Text -> Text -> Html ()
    textField lbl fid ph =
      div_
        [class_ "field"]
        ( label_ [for_ fid] (toHtml lbl)
            <> input_
              [ id_ fid
              , type_ "text"
              , autocomplete_ "off"
              , makeAttribute "placeholder" ph
              ]
        )

    footField :: Text -> Html () -> Html ()
    footField lbl ctl =
      div_ [class_ "foot-field"] (span_ [class_ "foot-label"] (toHtml lbl) <> ctl)

    -- «Порядок · Лимит · Публичная» одной строкой (на узком экране —
    -- с переносом). Поля и их поведение не меняются — меняется только
    -- место в разметке.
    resultRow :: Html ()
    resultRow =
      div_
        [class_ "result-row"]
        ( footField
            "Порядок"
            (div_ [id_ "e-sort", class_ "sort-box"] "")
            <> footField
              "Лимит"
              ( input_
                  [ id_ "e-limit"
                  , type_ "number"
                  , min_ "1"
                  , makeAttribute "placeholder" "без лимита"
                  , makeAttribute "aria-label" "Лимит треков"
                  ]
              )
            <> div_
              [class_ "foot-field foot-check"]
              ( input_ [id_ "e-public", type_ "checkbox"]
                  <> label_
                    [for_ "e-public", makeAttribute "title" "Видна всем пользователям"]
                    "Публичная"
              )
        )

    blockHead :: Text -> Html () -> Html ()
    blockHead t action =
      div_ [class_ "block-head"] (span_ [class_ "block-title"] (toHtml t) <> action)

    -- Свёрнутый по умолчанию блок технического кода: пока не раскрыт,
    -- пустая textarea не занимает место в колонке. Содержимое — файлы
    -- с диска (последняя опубликованная версия), а не live preview:
    -- предпросмотр живёт во вкладках «Правила | .mix | .nsp».
    collapsibleCode :: Text -> Text -> Text -> Text -> Maybe Text -> Html ()
    collapsibleCode heading fid rows label mCopy =
      detailsEl
        [class_ "block collapsible"]
        ( summaryEl
            [class_ "block-summary"]
            ( span_ [class_ "caret", makeAttribute "aria-hidden" "true"] ""
                <> span_ [class_ "block-title"] (toHtml heading)
            )
            <> div_
              [class_ "block-body"]
              ( div_
                  [class_ "block-row"]
                  ( span_ [class_ "block-label"] (toHtml label)
                      <> maybe
                        mempty
                        (\cid -> button_ [id_ cid, type_ "button", class_ "btn small"] "Копировать")
                        mCopy
                  )
                  <> textarea_
                    [ id_ fid
                    , class_ "code"
                    , rows_ rows
                    , readonly_ ""
                    , makeAttribute "spellcheck" "false"
                    , makeAttribute "aria-label" label
                    ]
                    ""
              )
        )

    tabButton :: Text -> Text -> Bool -> Html ()
    tabButton key label active =
      button_
        ( [ type_ "button"
          , class_ (if active then "tab active" else "tab")
          , makeAttribute "data-tab" key
          , makeAttribute "role" "tab"
          ]
            <> [makeAttribute "aria-selected" (if active then "true" else "false")]
        )
        (toHtml label)

    -- Карточка подборки: коллаж-обложка, название и число треков.
    playlistCard :: Html ()
    playlistCard =
      div_
        [class_ "pl-card"]
        ( div_
            [class_ "pl-cover", makeAttribute "aria-hidden" "true"]
            (span_ "" <> span_ "" <> span_ "" <> span_ "")
            <> div_
              [class_ "pl-info"]
              ( div_ [id_ "pl-title", class_ "pl-title"] "Без названия"
                  <> div_ [id_ "pl-meta", class_ "pl-meta"] "Лимит не задан"
              )
        )

    -- Деструктивное действие — в самом низу колонки (только если
    -- подборка опубликована).
    deleteFoot :: Html ()
    deleteFoot =
      maybe
        mempty
        ( \_ ->
            div_
              [class_ "col-foot"]
              ( button_
                  [ id_ "e-delete"
                  , type_ "button"
                  , class_ "btn danger wide"
                  , makeAttribute "title" "Перенести подборку в корзину"
                  ]
                  "Удалить из прода"
              )
        )
        mslug

------------------------------------------------------------------------------
-- Корзина
------------------------------------------------------------------------------

-- | Страница корзины с восстановлением и окончательным удалением.
trashPage :: [TrashEntry] -> Html ()
trashPage entries =
  layout
    "Muzlovar — корзина"
    ( div_ [class_ "toolbar"]
        ( h2_ "Корзина"
            <> spacer_
            <> a_ [href_ "/", class_ "btn"] "← К списку"
        )
        <> if null entries
          then
            div_
              [class_ "state empty"]
              "Корзина пуста. Удалённые подборки хранятся здесь до очистки."
          else table_ [class_ "list"] (theadRow <> tbody_ (foldMap row entries))
    )
  where
    theadRow =
      thead_
        ( tr_
            ( th_ "Идентификатор"
                <> th_ "Подборка"
                <> th_ "Удалена"
                <> th_ "Файлы"
                <> th_ "Действия"
            )
        )

    row :: TrashEntry -> Html ()
    row t =
      tr_
        ( td_ [class_ "file"] (toHtml (teId t))
            <> td_ [class_ "title"] (toHtml (teSlug t))
            <> td_ (toHtml (teDeletedAt t))
            <> td_
              [class_ "file"]
              ( maybe mempty toHtml (teMixFile t)
                  <> br_ []
                  <> maybe mempty toHtml (teNspFile t)
              )
            <> td_
              [class_ "actions"]
              ( button_
                  [type_ "button", makeAttribute "data-restore" (teId t)]
                  "Восстановить"
                  <> button_
                    [ type_ "button"
                    , class_ "danger small"
                    , makeAttribute "data-purge" (teId t)
                    ]
                    "Удалить навсегда"
              )
        )

------------------------------------------------------------------------------
-- Ошибка
------------------------------------------------------------------------------

-- | Простая страница ошибки (404 и т.п.).
errorPage :: Text -> Html ()
errorPage msg =
  layout
    "Muzlovar — ошибка"
    ( div_ [class_ "state error"]
        ( h2_ "Ошибка"
            <> p_ (toHtml msg)
            <> p_ (a_ [href_ "/"] "Вернуться к списку")
        )
    )

------------------------------------------------------------------------------
-- Время
------------------------------------------------------------------------------

-- | Человекочитаемая метка времени (или тире).
formatModified :: Maybe UTCTime -> Text
formatModified = \case
  Nothing -> "—"
  Just t -> T.pack (formatTime defaultTimeLocale "%Y-%m-%d %H:%M" t)
