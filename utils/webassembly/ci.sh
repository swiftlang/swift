#!/bin/bash

set -ex

SOURCE_PATH="$(cd "$(dirname $0)/../../.." && pwd)"
SWIFT_PATH=$SOURCE_PATH/swift
UTILS_PATH=$SWIFT_PATH/utils/webassembly

$SWIFT_PATH/utils/update-checkout --clone --scheme wasm --skip-repository swift
$UTILS_PATH/install-build-sdk.sh

export SCCACHE_CACHE_SIZE="50G"
export SCCACHE_DIR="$SOURCE_PATH/build-cache"

$UTILS_PATH/build-toolchain.sh --daily-snapshot
