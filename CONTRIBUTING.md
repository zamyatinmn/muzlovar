# Contributing to Muzlovar

Thanks for helping improve Muzlovar and Nspeller.

## Before opening a change

- Keep product behaviour, the Russian DSL, and the generated NSP format backward-compatible unless the change explicitly requires otherwise.
- Treat the Haskell field registry and typed model as the source of truth. Do not add a parallel field/operator table or NSP compiler in JavaScript.
- Keep unrelated refactors out of focused fixes.
- Never commit credentials, local playlist data, generated build output, or private infrastructure configuration.

For larger behaviour or format changes, open an issue first so the design and compatibility impact can be discussed.

## Development setup

Requirements are GHC 9.6 or newer and Cabal 3.10 or newer. Build and run the server with:

```bash
cabal build all
cabal run exe:muzlovar
```

The development server stores data in `./rules`, `./playlists`, and `./trash` unless the `MUZLOVAR_*_DIR` variables override those paths.

## Tests

Run the Haskell suite before submitting a change:

```bash
cabal test all
```

Changes to the editor, publishing flow, or browser layout should also run the relevant E2E suites:

```bash
cd muzlovar/e2e
npm ci
npm test
npm run test:publish-rename
npm run test:save-as-new
```

The E2E runner needs Chrome or Chromium. Set `MUZLOVAR_E2E_CHROME` if it cannot find the browser automatically.

## Pull requests

Describe the user-visible change, tests run, and any compatibility or migration concern. Add or update tests for behaviour changes. Keep generated `.nsp` golden files in sync only when the output change is deliberate and explained.
