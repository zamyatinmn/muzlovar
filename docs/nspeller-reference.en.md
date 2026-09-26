# Nspeller DSL reference (English)

[Русская версия](nspeller-reference.ru.md)

Nspeller compiles UTF-8 `.mix` recipes into Navidrome smart playlist `.nsp` JSON. The English and Russian spellings feed the **same parser, field registry, validated AST and compiler**. Equivalent recipes generate byte-for-byte identical `.nsp` files. Existing Russian recipes remain valid. The parser also accepts mixed spelling in one file; the Muzlovar generator writes one chosen dialect at a time.

## Build and commands

Use GHC 9.6+ and Cabal 3.10+:

```sh
cabal build all
cabal test all
cabal run nspeller -- check recipe.mix
cabal run nspeller -- build recipe.mix
cabal run nspeller -- build-all ./rules --output ./playlists
```

`build` writes a sibling `.nsp` unless `--output` is supplied. `build-all` checks all direct `*.mix` children before writing any output. The CLI accepts either dialect automatically; no language flag is needed to compile a source file. Diagnostics currently remain in Russian.

## A complete recipe

```text
# Personal favourites that have not been played recently
playlist "Forgotten favourites"
description "Loved tracks with at least three plays"
public

where all {
  loved
  playcount > 2
  not played within 90 days
  any {
    genre contains "rock"
    rating >= 4
  }
}

sort {
  playcount descending
  title ascending
}
limit 100
```

`playlist` and `where` are required exactly once. `description`, `public`, `sort` and `limit` are optional; duplicate sections are invalid. Sections may occur in any order. A group must contain at least one condition. Nested `all` and `any` groups are supported. `public` is a presence flag: omit it for a private playlist.

## Grammar

```ebnf
file          ::= { section }
section       ::= "playlist" string | "description" string | "public"
                | "where" group | "sort" ("random" | "{" sort-items "}")
                | "limit" integer
group         ::= ("all" | "any") "{" item { item } "}"
item          ::= group | "not played within" integer "days"
                | playlist-membership | field [ operator ]
playlist-membership ::= ("in playlist" | "not in playlist")
                         ("id" | "file") string
operator      ::= ("=" | "!=" | ">" | ">=" | "<" | "<=") value
                | "between" (number "and" number | date "and" date)
                | ("contains" | "does not contain" | "starts with"
                   | "ends with") value
                | "is missing" | "is present"
                | ("within" | "not within") integer "days"
                | ("before" | "after") date
sort-items    ::= sort-item { sort-item }
sort-item     ::= field ("ascending" | "descending")
value         ::= "true" | "false" | string | number | date
number        ::= [ "-" ] digits [ "." digits ]
integer       ::= [ "-" ] digits
date          ::= YYYY-MM-DD | '"' YYYY-MM-DD '"'
field         ::= identifier  (* see field catalog below *)
string        ::= '"' { ordinary-character | escape } '"'
escape        ::= "\\" ( '"' | "\\" | "n" | "t" | "r" )
comment       ::= "#" { character-to-end-of-line }
```

Whitespace, including CRLF and comments, separates tokens. English `day` is accepted as a singular alternative to `days`; the canonical renderer writes `days`. Dates must be real calendar dates. Strings, including playlist names, descriptions, search values, playlist IDs and file paths, are never translated. Numeric comparisons accept decimals where the field permits them. `limit` and day counts must be positive integers. Only a Boolean field may stand alone, where it means `= true`.

Multiword operators are parsed as complete phrases. For example, `does not contain` is distinct from `contains`, and `not within` is distinct from `within`. Invalid or incomplete phrases are errors. `not played within N days` is playback-specific shorthand for `lastplayed not within N days`: both compile to `notInTheLast` on `lastplayed`. The canonical English renderer uses the shorthand for that field. `not within N days` works with any date field.

## Field catalog

English DSL field tokens are the canonical Navidrome field names in the shared registry. The first column below is the exact token to write; its `.nsp` key is identical. The [Russian reference](nspeller-reference.ru.md#поля) lists every corresponding Russian token. The playlist reference is a separate grammar construct, not a normal field.

| Category | English DSL tokens | Value kind |
| --- | --- | --- |
| Logic | `loved`, `hascoverart`, `compilation` | Boolean |
| Logic | `rating`, `averagerating` | Number |
| History | `playcount` | Integer |
| History | `lastplayed`, `dateadded`, `dateloved`, `daterated` | Date |
| Metadata | `title`, `album`, `genre`, `explicitstatus`, `discsubtitle`, `comment`, `lyrics`, `sorttitle`, `sortalbum`, `sortartist`, `sortalbumartist`, `catalognumber` | Text |
| Metadata | `year`, `originalyear`, `releaseyear`, `tracknumber`, `discnumber` | Integer |
| Metadata | `date`, `originaldate`, `releasedate` | Date |
| ReplayGain | `rgtrackgain`, `rgtrackpeak`, `rgalbumgain`, `rgalbumpeak` | Number |
| Audio | `duration` | Number |
| Audio | `codec` | Text |
| Audio | `bitrate`, `bitdepth`, `samplerate`, `bpm`, `channels` | Integer |
| Files | `filepath`, `filetype` | Text |
| Files | `size` | Integer |
| Files | `datemodified` | Date |
| Files | `missing` | Boolean |
| Album | `albumcomment` | Text |
| Album | `albumrating`, `albumplaycount`, `albumduration`, `albumsongcount`, `albumsize` | Number (the count/rating/size fields are integers) |
| Album | `albumloved` | Boolean |
| Album | `albumlastplayed`, `albumdateloved`, `albumdaterated`, `albumdateadded`, `albumdatemodified` | Date |
| Artist | `artistrating`, `artistplaycount` | Integer |
| Artist | `artistloved` | Boolean |
| Artist | `artistlastplayed`, `artistdateloved`, `artistdaterated` | Date |
| MusicBrainz | `mbz_album_id`, `mbz_album_artist_id`, `mbz_artist_id`, `mbz_recording_id`, `mbz_release_track_id`, `mbz_release_group_id` | Text |
| Library | `library_id` | Integer |

There are 71 static fields and one playlist reference entry in the registry. The reference uses the `inPlaylist` / `notInPlaylist` NSP operators, represented in English DSL by `in playlist` / `not in playlist`. The `explicitstatus` values are exactly `"e"` (explicit), `"c"` (clean), and `""` (unknown). The parser also accepts legacy field aliases such as `lastPlayed`, `playCount`, and `replaygain_track_gain`; the explicit English renderer uses the canonical tokens above.

### Supported operators by field kind

| Field kind | Operators | Operand |
| --- | --- | --- |
| Text | `=`, `!=`, `contains`, `does not contain`, `starts with`, `ends with` | Quoted string |
| Number | `=`, `!=`, `>`, `>=`, `<`, `<=`, `between A and B` | Number(s) |
| Boolean | `=`, `!=`, or the bare field | `true` / `false` |
| Date | `=`, `!=`, `>`, `>=`, `<`, `<=`, `between A and B`, `before`, `after`, `within N days`, `not within N days` | Date(s) or positive day count |
| Playlist reference | `in playlist`, `not in playlist` | `id "…"` or `file "…"` |

`is missing` and `is present` additionally work on fields marked as presence-capable by the registry: selected text metadata and MusicBrainz IDs, `bpm`, `bitdepth`, and ReplayGain fields. `>=`/`<=` are lowered to Navidrome's supported conditions; compatible lower and upper bounds in one `all` group are combined into `inTheRange`.

`rating`, `albumrating`, and `artistrating` are integers from 0 through 5. Counts and sizes are nonnegative, and duration is nonnegative. Integer fields reject decimals. Contradictory conditions and out-of-domain values are errors; exact duplicates, redundant conditions and full-domain conditions produce nonblocking warnings. The same validation applies to both DSL dialects.

## Sort and playlist references

`sort random` maps to `"sort": "random"`. A sort block lists sortable field names followed by `ascending` or `descending`; for example, `sort { year descending title ascending }` maps to `"sort": "-year,title"`. A reference is written `in playlist id "playlist-id"` or `not in playlist file "other.nsp"`. Empty references are invalid; existence of the target playlist is not checked at compile time.

## Muzlovar language selection

The Web UI has its own Russian/English switch in the header. The `.mix` preview has an independent DSL language selector. An explicit DSL choice is saved in browser local storage; without one, a newly created recipe initially follows the UI language. When an existing `.mix` is opened, a recognised source dialect is selected for its preview. Switching either selector does not change the editor's recipe or the compiled `.nsp`; only an explicit publish/save writes files. API callers that omit `?dsl=en` continue to receive Russian `.mix` output. The `.nsp` format and field IDs do not depend on dialect.

## Navidrome and limitations

Put a generated `.nsp` in Navidrome's `PlaylistsPath` and wait for a scan. Personal fields such as `loved`, `rating`, `playcount`, and their album/artist counterparts are evaluated for the playlist owner. Arbitrary dynamic tags and unsupported external NSP nodes cannot be expressed in the current DSL; Muzlovar displays unsupported external playlists read-only. Server validation diagnostics are currently in Russian even when the source DSL is English.
