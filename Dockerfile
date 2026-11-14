# syntax=docker/dockerfile:1
#
# Yack: compiler image and static release tarball.
#
#   docker build -t yack .                 # runtime image (Ubuntu 24.04 + clang)
#   docker build --target release \
#       --build-arg YACK_COMMIT=$(git rev-parse --short HEAD) -o dist .
#
# The compiler itself is a static MLton binary built on Ubuntu 22.04; the
# runtime image only needs clang (for the LLVM IR that `yack build` emits),
# the C development files and sqlite3 for the `sql` package.

# ---------------------------------------------------------------------------
# Build stage: static Linux x86_64 `yack` binary.
# ---------------------------------------------------------------------------
FROM ubuntu:22.04 AS build

ARG YACK_COMMIT=unknown
ARG MLTON_PACKAGE=mlton-20241230-1.amd64-linux.ubuntu-20.04_static
ARG MLTON_URL=https://github.com/MLton/mlton/releases/download/on-20241230-release/${MLTON_PACKAGE}.tgz

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update \
 && apt-get install -y --no-install-recommends \
        make gcc libc6-dev libgmp-dev curl ca-certificates \
 && rm -rf /var/lib/apt/lists/*

RUN curl -fsSL "${MLTON_URL}" -o /tmp/mlton.tgz \
 && mkdir -p /opt \
 && tar -xzf /tmp/mlton.tgz -C /opt \
 && rm -f /tmp/mlton.tgz

# Keep this layer small and stable: only the compiler sources are needed.
COPY Makefile yack.mlb ./
COPY src/ src/

RUN make yack \
        MLTON=/opt/${MLTON_PACKAGE}/bin/mlton \
        GIT_COMMIT="${YACK_COMMIT}" \
        MLTONFLAGS="-default-ann 'allowExtendedTextConsts true' -link-opt -static"

RUN mkdir -p /out \
 && cp yack /out/yack \
 && strip /out/yack

# ---------------------------------------------------------------------------
# Release stage input: a self-contained yack-linux-x86_64/ directory.
# ---------------------------------------------------------------------------
COPY lib/ /release/yack-linux-x86_64/lib/
COPY README.md LICENSE /release/yack-linux-x86_64/
RUN cp /out/yack /release/yack-linux-x86_64/yack

# ---------------------------------------------------------------------------
# Static release artefact.  `-o dist` exports dist/yack-linux-x86_64/.
# ---------------------------------------------------------------------------
FROM scratch AS release
COPY --from=build /release/ /

# ---------------------------------------------------------------------------
# Default runtime image: clang 18 is recent enough for the opaque-pointer IR.
# ---------------------------------------------------------------------------
FROM ubuntu:24.04 AS runtime

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update \
 && apt-get install -y --no-install-recommends \
        clang libc6-dev binutils libsqlite3-dev ca-certificates \
 && rm -rf /var/lib/apt/lists/*

COPY --from=build /out/yack /usr/local/bin/yack
COPY --from=build /release/yack-linux-x86_64/lib /usr/local/lib/yack

# `YACK_LIB` is the directory that contains `yack/yack.toml`; the sibling
# packages (`web/`, `alpage/`, `net/`, `sql/`) are resolved from it too.
ENV YACK_LIB=/usr/local/lib/yack

WORKDIR /work
ENTRYPOINT ["yack"]
