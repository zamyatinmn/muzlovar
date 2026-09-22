# syntax=docker/dockerfile:1

##############################################################################
# Этап 1: сборка — GHC и cabal живут только здесь.
##############################################################################
FROM haskell:9.10.3-slim-bookworm AS builder

WORKDIR /build

# Сначала только манифест — слой с зависимостями кэшируется,
# пока не изменится nspeller.cabal. Таргеты exe: здесь нельзя указывать:
# cabal тогда конфигурирует и локальную библиотеку, а исходников в слое ещё нет.
COPY nspeller.cabal cabal.project ./
RUN cabal update \
    && cabal build --only-dependencies

# Затем исходники и сборка обоих executable.
COPY . .
RUN mkdir -p /out \
    && cabal build exe:nspeller exe:muzlovar \
    && cp "$(cabal list-bin exe:nspeller)" /out/nspeller \
    && cp "$(cabal list-bin exe:muzlovar)" /out/muzlovar

##############################################################################
# Этап 2: runtime — ни GHC, ни cabal, только бинарники и системные библиотеки.
##############################################################################
FROM debian:bookworm-slim

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        ca-certificates \
        curl \
        libffi8 \
        libgmp10 \
        tini \
    && rm -rf /var/lib/apt/lists/* \
    && useradd --system --uid 10001 --create-home muzlovar \
    && mkdir -p /rules /playlists /trash \
    && chown muzlovar:muzlovar /rules /playlists /trash

COPY --from=builder /out/nspeller /usr/local/bin/nspeller
COPY --from=builder /out/muzlovar /usr/local/bin/muzlovar

ENV MUZLOVAR_RULES_DIR=/rules \
    MUZLOVAR_PLAYLISTS_DIR=/playlists \
    MUZLOVAR_TRASH_DIR=/trash \
    MUZLOVAR_PORT=8765 \
    MUZLOVAR_HOST=* \
    LANG=C.UTF-8

USER muzlovar
WORKDIR /home/muzlovar

VOLUME ["/rules", "/playlists", "/trash"]
EXPOSE 8765

HEALTHCHECK --interval=30s --timeout=5s --start-period=15s --retries=3 \
    CMD curl -fsS http://127.0.0.1:8765/health || exit 1

# tini принимает SIGTERM от docker stop и передаёт его Warp
# (обработчик подписывается в muzlovar/Main.hs).
ENTRYPOINT ["/usr/bin/tini", "--", "/usr/local/bin/muzlovar"]
