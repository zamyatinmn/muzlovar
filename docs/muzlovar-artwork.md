# Muzlovar playlist artwork

Artwork is a Navidrome sidecar next to the published playlist, with the same
basename: `playlist.nsp` and `playlist.png`. Nothing is added to `.nsp`, `.mix`,
the publication state, or a database. No Navidrome API call or rescan is needed.

JPEG (`.jpg`, `.jpeg`), PNG, WebP and GIF are discovered automatically, including
manually placed sidecars and uppercase extensions. Uploaded bytes are saved
unchanged, with no resize, crop, quality change or conversion. Binary signatures
and container structure determine the format; filenames and client MIME headers
are not trusted. New JPEG uploads use `.jpg`; existing `.jpeg` sidecars retain
their extension when renamed.

## Editor

The square artwork block opens the file picker when clicked. Selecting a file
immediately uploads it via PUT for an existing playlist; the corner trash button
immediately calls DELETE. There is no artwork save button. After success, both the large
and compact previews update, as does the thumbnail in an open playlist-list tab.
GIF animation is handled by the browser. During an operation the editor shows a
small loading state and disables further artwork actions. Failures keep the
previous cover visible and show an error.

The list has an unnamed first column of 58 px, containing a centered 42×42 cover
or an empty cell. Titles align regardless of whether the playlist has artwork.

Artwork actions are independent of recipe saving and work even for read-only
external recipes. New and unpublished playlists accept a local File immediately:
both previews update, and replacement/removal stays local until publication.
After the first successful publication, PUT uploads the original bytes using the
final slug returned by the server. Failed publication retains the local File and
does not upload it. If only the upload fails, the playlist remains published and
the local preview stays visible with an error and a retry button. Ordinary recipe
saving does not retry that upload. Local selections survive retries within the
editor session, not page reloads. Save as new copies the original artwork bytes
after creating the new playlist, without modifying the source sidecar.

## API

All routes use the existing Basic Auth policy when configured. `slug` identifies
an existing `.nsp` basename and must be one safe path component. Clients cannot
provide a filesystem path. Separators, traversal, drive prefixes, Windows alternate
streams and symlink targets are rejected. URL-encode the slug in requests.

| Request | Contract |
| --- | --- |
| `GET /api/playlists/:slug/artwork` | `200` with original bytes and detected `image/jpeg`, `image/png`, `image/webp` or `image/gif`; `Cache-Control: no-cache`, `X-Content-Type-Options: nosniff`. `404` if playlist/artwork is absent. |
| `PUT /api/playlists/:slug/artwork` | Raw image bytes, **not multipart or JSON**. Maximum **10,485,760 bytes**, inclusive. Filename is inferred from slug and detected format. `200 {"artwork":true}`. |
| `DELETE /api/playlists/:slug/artwork` | Remove every supported sidecar with exactly this basename. `200 {"artwork":false}`; idempotent if the existing playlist has no cover. |

Errors use the existing `error`/`errors` JSON envelope. Oversized uploads return
`413` / `artwork_too_large`; unsupported or incomplete image containers return
`415` / `unsupported_artwork`. Missing playlists return `404`, unsafe paths `422`,
conflicting targets `409`, and filesystem failures `500`.

Playlist entries in list/detail responses have an additional boolean `artwork`.
The `.nsp` format and recipe DTO are unchanged.

## Filesystem lifecycle

Uploads use a unique temporary file in the playlist directory and rename it into
place. All previous supported sidecars are removed, even if their extension
differs. Recovery copies are retained until the operation succeeds; failure rolls
back the touched sidecars. The server serializes its filesystem operations to
avoid races with publishing, renaming, deleting and restoring playlists.

Rename preserves each existing sidecar's bytes and extension. A cover already
at the target basename causes a conflict rather than being silently overwritten.
When renaming a playlist with artwork, publication, cleanup and publication-state
updates participate in rollback. If rollback itself fails, the server returns an
explicit partial-operation error and retains recovery copies beside the files.
This is recovery for runtime errors, not a crash-proof filesystem transaction.

Deleting a playlist moves its matching sidecars into the same Trash entry as
`.mix`/`.nsp`. Restore brings them back; purge removes the Trash entry. Unrelated
images are untouched. Existing ownership checks for recipe renaming still apply.

## Verification

```sh
cabal test all
cd muzlovar/e2e
npm ci
npm test
npm run test:publish-rename
npm run test:save-as-new
npm run test:artwork
```

Manual checks against the configured music directory:

1. Open a published playlist without artwork, select an image, check that both
   previews and the list update immediately, then compare sidecar bytes with the
   original file. No recipe save should be needed.
2. Replace JPEG with PNG: only the new sidecar should remain.
3. Place a `.jpeg`, `.webp` or animated `.gif` beside an existing `.nsp`; reload
   the editor/list and check the image and GIF playback.
4. Rename a managed playlist and verify the sidecar has the new basename and
   its original extension. Check Navidrome picks it up without rescan.
5. Delete/restore a playlist through Trash; confirm artwork follows it and other
   images remain intact.
6. Try an unsupported file or a file larger than 10 MB; the previous cover should
   remain intact. Check both Russian and English labels and a narrow viewport.
