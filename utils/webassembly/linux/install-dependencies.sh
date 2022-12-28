#!/bin/bash

set -ex

DESTDIR="$1"
mkdir -p "$DESTDIR"

if [[ "$(cat /etc/system-release)" == *"Amazon Linux release 2"* ]]; then
  sudo yum -y install   \
    clang               \
    curl-devel          \
    gcc-c++             \
    git                 \
    glibc-static        \
    libbsd-devel        \
    libedit-devel       \
    libicu-devel        \
    libuuid-devel       \
    libxml2-devel       \
    ncurses-compat-libs \
    ncurses-devel       \
    ninja-build         \
    pexpect             \
    pkgconfig           \
    procps-ng           \
    python              \
    python-devel        \
    python-pkgconfig    \
    python-six          \
    python3-devel       \
    rsync               \
    sqlite-devel        \
    swig                \
    tzdata              \
    uuid-devel          \
    wget                \
    which

  if [ ! -e /usr/local/bin/ninja ]; then
    sudo ln -s /usr/bin/ninja-build /usr/local/bin/ninja
  fi
else
  echo "skip"
fi

SOURCE_PATH="$( cd "$(dirname $0)/../../../.." && pwd )" 
SWIFT_PATH=$SOURCE_PATH/swift

mkdir -p "$DESTDIR/bin"

# Install wasmer
# FIXME: Wasmer doesn't support linux-aarch64, consider using a different WASI-compatible runtime.
if [ "$(uname -m)" != "aarch64" ]; then
  export WASMER_DIR="${WASMER_DIR:-$HOME/.wasmer}"
  if [ ! -e "$WASMER_DIR" ]; then
    curl https://get.wasmer.io -sSfL | sh -s "2.1.1"
    ln -sf "$WASMER_DIR/bin/wasmer" "$DESTDIR/bin/wasmer"
  fi
fi

install_cmake() {
  local workdir="$(mktemp -d)"
  curl -L -o "$workdir/install_cmake.sh" "https://github.com/Kitware/CMake/releases/download/v3.22.1/cmake-3.22.1-linux-$(uname -m).sh"
  chmod +x "$workdir/install_cmake.sh"
  mkdir -p "$DESTDIR/cmake"
  "$workdir/install_cmake.sh" --skip-license --prefix="$DESTDIR/cmake"
  ln -sf "$DESTDIR/cmake/bin"/* "$DESTDIR/bin"
}

if [ ! -e "$DESTDIR/cmake/bin/cmake" ]; then
  install_cmake
fi

# Install sccache

if [ -z $(which sccache) ]; then
  mkdir "$DESTDIR/sccache" && cd "$DESTDIR/sccache"
  curl -L "https://github.com/mozilla/sccache/releases/download/v0.3.0/sccache-v0.3.0-$(uname -m)-unknown-linux-musl.tar.gz" | \
    tar xz --strip-components 1
  chmod +x "$DESTDIR/sccache/sccache"
  ln -sf "$DESTDIR/sccache/sccache" "$DESTDIR/bin/sccache"
fi

cd $SWIFT_PATH
./utils/update-checkout --clone --scheme wasm --skip-repository swift
