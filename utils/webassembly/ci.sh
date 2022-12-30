#!/bin/bash

set -ex

SOURCE_PATH="$( cd "$(dirname $0)/../../.." && pwd  )" 
SWIFT_PATH=$SOURCE_PATH/swift
UTILS_PATH=$SWIFT_PATH/utils/webassembly

case $(uname -s) in
  Darwin)
    HOST_SUFFIX=macosx-$(uname -m)
  ;;
  Linux)
    HOST_SUFFIX=linux-$(uname -m)
  ;;
  *)
    echo "Unrecognised platform $(uname -s)"
    exit 1
  ;;
esac

BUILD_SCRIPT=$UTILS_PATH/build-toolchain.sh
RUN_TEST_BIN=$SWIFT_PATH/utils/run-test
TARGET_STDLIB_BUILD_DIR=$SOURCE_PATH/target-build/swift-stdlib-wasi-wasm32
BUILD_SDK_PATH="$SOURCE_PATH/build-sdk"

export WASMER_DIR="$BUILD_SDK_PATH/wasmer"

$UTILS_PATH/install-dependencies.sh "$BUILD_SDK_PATH"
$UTILS_PATH/install-build-sdk.sh

export PATH="$BUILD_SDK_PATH/bin:$PATH"
export SCCACHE_CACHE_SIZE="50G"
export SCCACHE_DIR="$SOURCE_PATH/build-cache"

$BUILD_SCRIPT

# workaround: host target test directory is necessary to use run-test
mkdir -p "$TARGET_STDLIB_BUILD_DIR/test-$HOST_SUFFIX"
