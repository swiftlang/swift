#!/bin/bash

set -eux

SOURCE_PATH="$( cd "$(dirname "$0")/../../../" && pwd  )"

cd "$SOURCE_PATH"

WASI_SDK_URL="https://github.com/swiftwasm/wasi-sdk/releases/download/0.2.2-swiftwasm/dist-$2.zip"

workdir=$(mktemp -d)
pushd $workdir

wget -O dist-wasi-sdk.zip "$WASI_SDK_URL"
unzip -u dist-wasi-sdk.zip -d .

WASI_SDK_TAR_PATH=$(find "$workdir" -type f -name "wasi-sdk-*")
WASI_SDK_FULL_NAME=$(basename "$WASI_SDK_TAR_PATH" -"$1".tar.gz)
tar xfz "$WASI_SDK_TAR_PATH"
popd

rm -rf $SOURCE_PATH/wasi-sdk
mv "$workdir/$WASI_SDK_FULL_NAME" $SOURCE_PATH/wasi-sdk
