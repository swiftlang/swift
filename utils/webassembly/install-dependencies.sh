#!/bin/bash

set -ex

install_wasmer() {
  if [ "$(uname -m)" != "aarch64" ]; then
    export WASMER_DIR="${WASMER_DIR:-$HOME/.wasmer}"
    if [ ! -e "$WASMER_DIR" ]; then
      curl https://get.wasmer.io -sSfL | sh -s "2.1.1"
      ln -sf "$WASMER_DIR/bin/wasmer" "$DESTDIR/bin/wasmer"
    fi
  else
    echo "FIXME: Wasmer doesn't support linux-aarch64, consider using a different WASI-compatible runtime."
  fi
}

install_linux_cmake() {
  local version="3.22.1"
  local workdir="$(mktemp -d)"
  curl -L -o "$workdir/install_cmake.sh" "https://github.com/Kitware/CMake/releases/download/v$version/cmake-$version-linux-$(uname -m).sh"
  chmod +x "$workdir/install_cmake.sh"
  mkdir -p "$DESTDIR/cmake"
  "$workdir/install_cmake.sh" --skip-license --prefix="$DESTDIR/cmake"
  ln -sf "$DESTDIR/cmake/bin"/* "$DESTDIR/bin"
}

install_linux_sccache() {
  local version="v0.3.0"
  mkdir "$DESTDIR/sccache"
  (
    cd "$DESTDIR/sccache" && \
    curl -L "https://github.com/mozilla/sccache/releases/download/$version/sccache-$version-$(uname -m)-unknown-linux-musl.tar.gz" | \
    tar xz --strip-components 1
  )
  chmod +x "$DESTDIR/sccache/sccache"
  ln -sf "$DESTDIR/sccache/sccache" "$DESTDIR/bin/sccache"
}

DESTDIR="$1"
mkdir -p "$DESTDIR"
mkdir -p "$DESTDIR/bin"

SOURCE_PATH="$(cd "$(dirname $0)/../../.." && pwd)"
SWIFT_PATH=$SOURCE_PATH/swift
cd $SWIFT_PATH

case $(uname -s) in
  Darwin)
    brew bundle
  ;;
  Linux)
    if [ ! -e "$DESTDIR/bin/cmake" ]; then
      install_linux_cmake
    fi
    if [ ! -e "$DESTDIR/bin/sccache" ]; then
      install_linux_sccache
    fi
  ;;
  *)
    echo "Unrecognised platform $(uname -s)"
    exit 1
  ;;
esac
install_wasmer

cd $SWIFT_PATH
./utils/update-checkout --clone --scheme wasm --skip-repository swift
