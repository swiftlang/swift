#!/bin/bash

set -eux

SOURCE_PATH="$( cd "$(dirname "$0")/../../../" && pwd  )"
BUILD_SDK_PATH="$SOURCE_PATH/build-sdk"
PATCHES="$(cd "$(dirname "$0")/patches" && pwd)"

install_libxml2() {
  LIBXML2_URL="https://github.com/swiftwasm/libxml2-wasm/releases/download/1.0.0/libxml2-wasm32-unknown-wasi.tar.gz"
  curl -L "$LIBXML2_URL" | tar xz
  rm -rf "$BUILD_SDK_PATH/libxml2"
  mv libxml2-wasm32-unknown-wasi "$BUILD_SDK_PATH/libxml2"
}

install_icu() {
  ICU_URL="https://github.com/swiftwasm/icu4c-wasi/releases/download/0.5.0/icu4c-wasi.tar.xz"
  curl -L "$ICU_URL" | tar Jx
  rm -rf "$BUILD_SDK_PATH/icu"
  mv icu_out "$BUILD_SDK_PATH/icu"
}

install_wasi-sysroot() {
  WASI_SYSROOT_URL="https://github.com/swiftwasm/wasi-sdk-build/releases/download/wasi-sdk-14%2Bswiftwasm-2022-03-13/wasi-sysroot.tar.gz"

  curl -L "$WASI_SYSROOT_URL" | tar xz

  mv "wasi-sysroot" "$BUILD_SDK_PATH/wasi-sysroot"
  patch -p1 -d "$BUILD_SDK_PATH/wasi-sysroot" < "$PATCHES/wasi-sysroot"/*.patch
}

workdir=$(mktemp -d)
pushd "$workdir"

mkdir -p "$BUILD_SDK_PATH"

if [ ! -e "$BUILD_SDK_PATH/libxml2" ]; then
  install_libxml2
fi

if [ ! -e "$BUILD_SDK_PATH/icu" ]; then
  install_icu
fi

if [ ! -e "$BUILD_SDK_PATH/wasi-sysroot" ]; then
  install_wasi-sysroot
fi
