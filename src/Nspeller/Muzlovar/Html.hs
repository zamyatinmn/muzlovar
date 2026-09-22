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
  , indexPage
  , editorPage
  , trashPage
  , errorPage
  , formatModified
  ) where

import Data.Maybe (fromMaybe)
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
layout title mainContent =
  doctypehtml_ (html_ [lang_ "ru"] page)
  where
    page =
      head_
        ( meta_ [charset_ "utf-8"]
            <> meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
            <> meta_ [name_ "color-scheme", content_ "dark"]
            <> title_ (toHtml title)
            <> link_ [rel_ "stylesheet", href_ "/static/muzlovar.css"]
        )
        <> body_
          ( header_
              [class_ "app"]
              ( h1_ [class_ "brand"] "Muzlovar"
                  <> nav_
                    ( a_ [href_ "/"] "Подборки"
                        <> a_ [href_ "/new"] "Новая"
                        <> a_ [href_ "/trash"] "Корзина"
                    )
              )
              <> main_ mainContent
              <> footer_
                [class_ "app"]
                "Muzlovar — редактор умных подборок Navidrome. Файлы .mix и .nsp \
                \хранятся на диске; база данных Navidrome не изменяется напрямую."
              <> div_ [id_ "toasts", makeAttribute "aria-live" "polite"] ""
              <> script_ [src_ "/static/sortable.min.js"] ("" :: Text)
              <> script_ [src_ "/static/muzlovar.js"] ("" :: Text)
              <> dialogs
          )

    dialogs = deleteDialog <> overwriteDialog

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
editorPage :: Maybe Text -> Html ()
editorPage mslug =
  layout
    title
    ( div_
        [id_ "editor", makeAttribute "data-slug" (fromMaybe "" mslug)]
        ( div_
            [id_ "e-status", class_ "state loading", makeAttribute "hidden" "hidden"]
            ""
            <> div_
              [class_ "toolbar"]
              ( a_ [href_ "/", class_ "btn"] "← К списку"
                  <> spacer_
                  <> button_ [id_ "e-save", type_ "button"] "Проверить"
                  <> button_ [id_ "e-publish", type_ "button", class_ "primary"] "Опубликовать"
                  <> maybe mempty (\_ -> button_ [id_ "e-delete", type_ "button", class_ "danger"] "Удалить") mslug
              )
            <> div_ [class_ "editor"]
              ( div_
                  ( panel
                      "Метаданные"
                      ( div_
                          [class_ "fields"]
                          ( field "Название" (input_ [id_ "e-name", type_ "text", autocomplete_ "off"])
                              <> field "Описание" (input_ [id_ "e-desc", type_ "text", autocomplete_ "off"])
                              <> field "Лимит треков" (input_ [id_ "e-limit", type_ "number", min_ "1"])
                              <> div_
                                [class_ "field"]
                                ( label_ [for_ "e-public"] "Публичная"
                                    <> div_
                                      [class_ "check"]
                                      ( input_ [id_ "e-public", type_ "checkbox"]
                                          <> span_ "видна всем пользователям"
                                      )
                                )
                          )
                          <> p_
                            [id_ "e-personal", class_ "notice", makeAttribute "hidden" "hidden"]
                            ""
                      )
                      <> panel
                        "Палитра полей"
                        ( p_ [class_ "meta-line"] "Перетащите чип в дерево или нажмите на него."
                            <> div_ [id_ "e-palette", class_ "palette"] ""
                        )
                      <> panel
                        "Условия"
                        ( div_ [id_ "e-tree"] ""
                            <> div_ [id_ "e-errors", makeAttribute "hidden" "hidden"] ""
                        )
                      <> panel
                        "Сортировка и лимит"
                        ( div_ [id_ "e-sort", class_ "palette"] "" )
                  )
                  <> div_
                    ( panel
                        "Предпросмотр .mix"
                        ( div_ [id_ "e-preview-wrap", makeAttribute "hidden" "hidden"] ""
                            <> pre_ [id_ "e-preview", class_ "preview"] ""
                        )
                        <> panel
                          "Исходные файлы"
                          ( label_ [for_ "e-mix"] ".mix на диске"
                              <> textarea_ [id_ "e-mix", rows_ "8", readonly_ ""] ""
                              <> label_ [for_ "e-nsp"] ".nsp на диске"
                              <> textarea_ [id_ "e-nsp", rows_ "8", readonly_ ""] ""
                          )
                    )
              )
        )
    )
  where
    title = case mslug of
      Just s -> "Muzlovar — " <> s
      Nothing -> "Muzlovar — новая подборка"

    panel :: Text -> Html () -> Html ()
    panel h c = section_ [class_ "panel"] (h_ h <> c)

    h_ :: Text -> Html ()
    h_ t = h2_ (toHtml t)

    field :: Text -> Html () -> Html ()
    field lbl ctl = div_ [class_ "field"] (label_ (toHtml lbl) <> ctl)

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
