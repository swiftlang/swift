#/bin/bash

set -ex

SOURCE_PATH="$( cd "$(dirname $0)/../../.." && pwd  )" 
SWIFT_PATH=$SOURCE_PATH/swift
UTILS_PATH=$SWIFT_PATH/utils/webassembly
if [[ "$(uname)" == "Linux" ]]; then
  DEPENDENCIES_SCRIPT=$UTILS_PATH/linux/install-dependencies.sh
else
  DEPENDENCIES_SCRIPT=$UTILS_PATH/macos/install-dependencies.sh
fi

BUILD_SCRIPT=$UTILS_PATH/build-toolchain.sh
RUN_TEST_BIN=$SWIFT_PATH/utils/run-test
BUILD_DIR=$SOURCE_PATH/build/Ninja-ReleaseAssert

$DEPENDENCIES_SCRIPT

export PATH="$HOME/.wasmer/bin:$PATH"

export SCCACHE_CACHE_SIZE="50G"
export SCCACHE_DIR="$SOURCE_PATH/build-cache"

$BUILD_SCRIPT

if [[ "$(uname)" == "Darwin" ]]; then
  # workaround: host target test directory is necessary to use run-test
  mkdir -p $BUILD_DIR/swift-macosx-x86_64/test-macosx-x86_64
fi

$RUN_TEST_BIN --build-dir $BUILD_DIR --target wasi-wasm32 test/stdlib/

if [[ "$(uname)" == "Linux" ]]; then
  echo "Skip running test suites for Linux"
else
  # Run test but ignore failure temporarily
  $BUILD_SCRIPT -t || true
fi
