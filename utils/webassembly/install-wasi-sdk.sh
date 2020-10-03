#!/bin/bash

set -ex

SOURCE_PATH="$( cd "$(dirname "$0")/../../../" && pwd  )"

cd "$SOURCE_PATH"

WASI_SDK_URL="https://github.com/swiftwasm/wasi-sdk/releases/download/0.2.2-swiftwasm/dist-$2.zip"

[ ! -e dist-wasi-sdk.zip ] && \
  wget -O dist-wasi-sdk.zip "$WASI_SDK_URL"
unzip -u dist-wasi-sdk.zip -d .
WASI_SDK_TAR_PATH=$(find . -type f -name "wasi-sdk-*")
WASI_SDK_FULL_NAME=$(basename "$WASI_SDK_TAR_PATH" -"$1".tar.gz)
tar xfz "$WASI_SDK_TAR_PATH"
rm -rf ./wasi-sdk
mv "$WASI_SDK_FULL_NAME" ./wasi-sdk
