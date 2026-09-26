English | [Русский](README.ru.md)

# Muzlovar

[![CI](https://github.com/zamyatinmn/muzlovar/actions/workflows/ci.yml/badge.svg)](https://github.com/zamyatinmn/muzlovar/actions/workflows/ci.yml)

**A self-hosted visual smart-playlist editor for Navidrome.**

Muzlovar lets you build complex [Navidrome smart playlists](https://www.navidrome.org/docs/usage/features/smart-playlists/) without writing `.nsp` JSON by hand. Arrange typed rules in a browser, preview the generated `.mix` and `.nsp`, validate the result, and publish it to Navidrome's playlist directory.

The browser edits a DTO; it does not contain a second playlist compiler. Muzlovar's server and the bundled **Nspeller** CLI both use the same Haskell field registry, typed AST, validation rules, renderer, and Navidrome model.

**Language:** The Muzlovar web UI and Nspeller `.mix` DSL both support Russian and English. Choose the UI language in the header and the independent DSL language in the `.mix` preview; the browser saves explicit choices locally. Both dialects compile to identical `.nsp` output, and existing Russian recipes remain supported. The screenshot below shows the Russian interface.

## Highlights

- Visual rule builder with nested **ALL/ANY** groups, palette search, keyboard-friendly controls, and pointer-based drag and drop.
- Live canonical `.mix` and `.nsp` previews backed by server-side validation.
- Rules for metadata, listening history, ratings, favourites, album and artist statistics, audio properties, identifiers, and playlist references.
- Safe publishing with collision handling, atomic file replacement, persisted ownership checks, and rollback on write failure.
- Rename, save-as-new, recoverable trash, restore, and permanent deletion workflows.
- Existing NSP files with unsupported nodes remain visible but are treated as read-only external playlists.
- Optional Basic Auth and optional Subsonic integration for looking up and deleting the matching Navidrome playlist entity.
- Multi-stage Docker image, a small non-root runtime, Haskell unit/golden/property/integration tests, and browser E2E tests.
- Nspeller CLI for checking one `.mix`, compiling one file, or validating and compiling a directory as a batch.

![Muzlovar visual smart-playlist editor](docs/images/muzlovar-editor.png)

## Quick start with Docker

After the first release tag, ready-to-run images will be available as [`ghcr.io/zamyatinmn/muzlovar`](https://github.com/zamyatinmn/muzlovar/pkgs/container/muzlovar). No Haskell toolchain is required to run them.

Pull it directly:

```bash
docker pull ghcr.io/zamyatinmn/muzlovar:latest
```

Or clone the repository for the ready-to-use Compose configuration:

```bash
git clone https://github.com/zamyatinmn/muzlovar.git
cd muzlovar
docker compose up -d
```

Open <http://localhost:8765>. The default Compose file pulls `ghcr.io/zamyatinmn/muzlovar:latest` and uses Docker-managed volumes, so after the first image release it works on a fresh clone without GHC, Cabal, host-directory preparation, or local image builds.

Authentication is disabled in the zero-config setup. Keep the service on a trusted network, or create a `.env` from [`.env.example`](.env.example) and set both `MUZLOVAR_USERNAME` and `MUZLOVAR_PASSWORD` before exposing it through a reverse proxy.

To make published playlists visible to Navidrome, mount the same `muzlovar_playlists` volume at Navidrome's `PlaylistsPath`, or replace that named volume with a bind mount to your existing playlist directory. The container runs as UID `10001`, so a bind-mounted host directory must be writable by that UID.

```yaml
services:
  navidrome:
    volumes:
      - muzlovar_playlists:/playlists
    environment:
      ND_PLAYLISTSPATH: /playlists

volumes:
  muzlovar_playlists:
    name: muzlovar_playlists
```

The image contains both `muzlovar` and `nspeller`; GHC and Cabal stay in the build stage. The runtime process is non-root and is supervised by `tini`. `GET /health` is used for the container health check.

### Build the container from source

Local container development is kept separate from the release image:

```bash
docker compose -f docker-compose.yaml -f docker-compose.dev.yaml up -d --build
```

The override builds the same production Dockerfile locally and tags it as `muzlovar:local`. It does not push anything to GHCR.

## How it works

```text
Visual editor DTO ─┐
                   ├─> typed Haskell AST -> validation -> canonical .mix
RU/EN .mix DSL ────┘                                      |
                                                            v
                                                Navidrome model -> .nsp JSON
```

The field registry is the single source of truth for field names, types, allowed operators, numeric limits, UI palette groups, and sort support. `/api/schema` exposes that model to the UI. `/api/validate` converts the current editor DTO into the same typed AST used by Nspeller, then returns canonical `.mix` and `.nsp` previews. JavaScript manages browser interactions only; it does not duplicate NSP generation.

Muzlovar is server-rendered HTML (Scotty + Lucid) with vanilla JavaScript and CSS. There is no client framework and no production Node.js dependency.

## Product tour

Muzlovar provides three main views:

- **Playlists** lists `.mix` and `.nsp` files, metadata, rule summaries, publication state, modification time, and `managed`, `external`, or `broken` status.
- **Editor** combines an ingredient palette, nested rule tree, sorting and limit controls, validation feedback, and live `.mix`/`.nsp` previews. Published playlists can be updated, renamed, or saved as a new playlist.
- **Trash** contains recoverable deletions and supports restore or permanent removal.

Deletion moves managed files to Muzlovar's trash. It does not edit Navidrome's database directly. If all three Subsonic settings are present, Muzlovar can also call `deletePlaylist`; otherwise delete the imported playlist in Navidrome's UI if necessary.

Personal fields such as favourites, ratings, play counts, and last-played dates are evaluated by Navidrome for the playlist owner. The same NSP may therefore produce different results for different owners.

## Configuration

| Variable | Default | Purpose |
| --- | --- | --- |
| `MUZLOVAR_USERNAME` | unset | Optional Basic Auth username; must be set together with the password |
| `MUZLOVAR_PASSWORD` | unset | Optional Basic Auth password; must be set together with the username |
| `MUZLOVAR_RULES_DIR` | `./rules` | Source `.mix` directory |
| `MUZLOVAR_PLAYLISTS_DIR` | `./playlists` | Published `.nsp` directory, normally Navidrome's `PlaylistsPath` |
| `MUZLOVAR_TRASH_DIR` | `./trash` | Recoverable trash directory |
| `MUZLOVAR_PORT` | `8765` | HTTP port, from 1 to 65535 |
| `MUZLOVAR_HOST` | `*` | Bind address |
| `MUZLOVAR_SUBSONIC_URL` | unset | Optional Subsonic API base URL |
| `MUZLOVAR_SUBSONIC_USER` | unset | Subsonic username |
| `MUZLOVAR_SUBSONIC_PASSWORD` | unset | Subsonic password |

Basic Auth is enabled only when both credentials are non-empty. With neither credential set, the application is unauthenticated; with only one set, startup fails. `/health` and `/static/*` remain public when authentication is enabled.

The Subsonic adapter is enabled only when URL, username, and password are all supplied. Credentials and URL query strings are scrubbed from connection-error logs.

### Running from source

Requirements: GHC 9.6 or newer (9.10.3 is used by the container and CI), Cabal 3.10 or newer, and Hackage access for the first build.

```bash
cabal build
cabal run exe:muzlovar
```

The local development server uses `./rules`, `./playlists`, and `./trash` by default and listens on <http://127.0.0.1:8765/>.

Release tags matching `vX.Y.Z` publish images for `linux/amd64`. A tag such as `v1.2.3` produces `1.2.3`, `1.2`, `1`, and `latest`. Normal pushes to `master` never publish or replace container tags. ARM64 is not published yet because the current `crypton` dependency does not compile successfully for that target in the container build.

## Nspeller: the compiler behind Muzlovar

Nspeller turns the human-readable Russian or English `.mix` DSL into Navidrome `.nsp` JSON. Files are UTF-8 and may contain `#` line comments. UI language and DSL language can be selected separately.

```text
подборка "Forgotten favourites"
описание "Loved tracks that have not played recently"

где все {
  любимое
  прослушиваний > 2
  не звучало 90 дней
}

порядок случайный
лимит 100
```

### CLI

```bash
# Validate without writing output
nspeller check examples/forgotten.mix

# Compile next to the source file
nspeller build examples/forgotten.mix

# Compile to an explicit path
nspeller build examples/forgotten.mix --output /path/to/forgotten.nsp

# Validate every direct child *.mix first, then compile the whole batch
nspeller build-all ./rules --output ./playlists
```

`build-all` is non-recursive. If any source is invalid, every error is reported and no output is changed. Successful writes use a temporary file in the destination directory followed by a rename; orphaned `.nsp` files are not deleted.

### DSL structure

Every file needs exactly one `подборка` (playlist name) section and one `где` (rules) section. Optional sections add `описание` (comment), `публичная` (`public: true`), `порядок` (sort), and `лимит` (positive limit). Sections may appear in any order.

```ebnf
file        ::= { section }
section     ::= "подборка" string
              | "описание" string
              | "публичная"
              | "где" group
              | sort
              | "лимит" integer
group       ::= ("все" | "любое") "{" condition { condition } "}"
condition   ::= group | expression | playlist-membership | not-played
```

Groups compile to Navidrome's `all` and `any` nodes and may be nested without a fixed depth limit. Supported operations include equality/inequality, numeric and date comparisons, ranges, text contains/prefix/suffix operations, missing/present checks where Navidrome supports them, relative dates, playlist membership, and the `любимое` favourite shorthand.

Nspeller lowers `>=` and `<=` into valid Navidrome expressions because NSP has no native inclusive comparison. Compatible lower and upper bounds in an `all` group may be folded into `inTheRange`.

The registry currently covers track metadata; listening-history and rating fields; album and artist aggregate fields; audio/file properties; MusicBrainz identifiers; and playlist references. It rejects undocumented or unsupported combinations before JSON generation. Dynamic Navidrome tags and arbitrary custom fields are not supported.

For the English grammar and field catalog, see the [English DSL reference](docs/nspeller-reference.en.md). The [Russian reference](docs/nspeller-reference.ru.md) retains the detailed Russian grammar, warning rules, examples, and error categories. The runnable source/expected-output pairs are in [`examples/`](examples/).

### Validation and warnings

Validation collects all errors in a file instead of stopping at the first one. It checks required and duplicate sections, field/operator compatibility, value types and ranges, integer-only fields, sort fields, non-empty playlist references, contradictory numeric conditions, and positive limits.

Non-fatal warnings cover exact duplicates, redundant bounds, and conditions that cover an entire known domain. Conditions in separate `any` branches are not treated as contradictions or duplicates.

The validated AST is type-indexed: a text condition requires `FieldRef Text`, a number condition requires `FieldRef Scientific`, and so on. Invalid field/operator combinations cannot be represented after validation.

### Output

Generated NSP is ordinary JSON with no comments. Optional keys are omitted when absent. Output is deterministic, pretty-printed with two-space indentation, and ends with a newline. Golden tests pin the current byte representation, although JSON object key order is not part of the external contract.

## HTTP API

- `GET /api/schema` — fields, operators, constraints, and palette groups from the Haskell registry.
- `GET /api/playlists` and `GET /api/playlists/:slug` — playlist list and details.
- `POST /api/validate` — validation plus canonical `.mix` and `.nsp` previews; no disk write.
- `POST /api/playlists` — publish a new playlist.
- `PUT /api/playlists/:slug[?overwrite=1]` — update or rename a managed playlist.
- `DELETE /api/playlists/:slug` — move a managed playlist to trash.
- `GET /api/trash`, `POST /api/trash/:id/restore`, `DELETE /api/trash/:id` — trash lifecycle.
- `GET /health` — public liveness check.

Publishing writes `.mix` and `.nsp` as one managed pair. A persisted `.muzlovar-published.json` file records the paths and FNV-1a hashes of bytes written by Muzlovar. Rename cleanup is allowed only when that record still matches; changed, foreign, symlinked, or out-of-directory files block cleanup instead of being silently removed.

## Tests

```bash
cabal test all
cabal build all
```

The Haskell suite contains unit, golden, QuickCheck property, DTO/AST round-trip, schema-consistency, store, and WAI integration tests. It covers atomic write/rollback behaviour, collisions, path traversal, symlinks, managed/external/broken states, Basic Auth, API errors, rename, and the trash lifecycle.

Browser E2E tests require Node.js and a local Chrome/Chromium executable:

```bash
cd muzlovar/e2e
npm ci
npm test
npm run test:publish-rename
npm run test:save-as-new
```

Set `MUZLOVAR_E2E_CHROME` to a browser executable when it cannot be discovered automatically. Set `MUZLOVAR_E2E_SKIP_BUILD=1` to reuse an already-built `muzlovar` binary.

## Current limitations

- Muzlovar is file-backed; it has no application database or file watcher.
- Subsonic integration only looks up playlists and deletes a playlist entity. Creation and updates happen by writing `.nsp` files for Navidrome to scan.
- Nspeller DSL supports Russian and English. Server diagnostic messages remain Russian; unknown server diagnostics are shown as received.
- There is no music-file analysis, recommendation engine, ML, Android integration, or recursive `build-all`.
- `limitPercent`, general negation, list operands, and arbitrary custom Navidrome fields are not supported.
- External NSP trees containing nodes that cannot be represented by the DSL are read-only.

## Project layout

```text
app/                         Nspeller CLI entry point
muzlovar/                    Muzlovar executable, static assets, and E2E tests
src/Nspeller/                parser, typed model, validation, rendering, CLI
src/Nspeller/Muzlovar/       server, HTML, store, DTOs, and Subsonic adapter
examples/                    complete .mix -> .nsp examples
test/                        Haskell unit, golden, property, and integration tests
docs/nspeller-reference.ru.md exhaustive Russian-language technical reference
```

## Contributing and security

See [`CONTRIBUTING.md`](CONTRIBUTING.md) for the development workflow. Please report vulnerabilities according to [`SECURITY.md`](SECURITY.md), not in a public issue.

Muzlovar and Nspeller are available under the [MIT License](LICENSE).
