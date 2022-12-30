#!/bin/bash

set -ex

SOURCE_PATH="$(cd "$(dirname $0)/../../.." && pwd)"
SWIFT_PATH=$SOURCE_PATH/swift
UTILS_PATH=$SWIFT_PATH/utils/webassembly
BUILD_SCRIPT=$UTILS_PATH/build-toolchain.sh
BUILD_SDK_PATH="$SOURCE_PATH/build-sdk"

export WASMER_DIR="$BUILD_SDK_PATH/wasmer"

$UTILS_PATH/install-dependencies.sh "$BUILD_SDK_PATH"
$UTILS_PATH/install-build-sdk.sh

export PATH="$BUILD_SDK_PATH/bin:$PATH"
export SCCACHE_CACHE_SIZE="50G"
export SCCACHE_DIR="$SOURCE_PATH/build-cache"

$UTILS_PATH/build-toolchain.sh
