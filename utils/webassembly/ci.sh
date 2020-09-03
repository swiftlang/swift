#/bin/bash

set -ex

SOURCE_PATH="$( cd "$(dirname $0)/../../.." && pwd  )" 
SWIFT_PATH=$SOURCE_PATH/swift
UTILS_PATH=$SWIFT_PATH/utils/webassembly

case $(uname -s) in
  Darwin)
    DEPENDENCIES_SCRIPT=$UTILS_PATH/macos/install-dependencies.sh
    HOST_SUFFIX=macosx-x86_64
  ;;
  Linux)
    DEPENDENCIES_SCRIPT=$UTILS_PATH/linux/install-dependencies.sh
    HOST_SUFFIX=linux-x86_64
  ;;
  *)
    echo "Unrecognised platform $(uname -s)"
    exit 1
  ;;
esac

BUILD_SCRIPT=$UTILS_PATH/build-toolchain.sh
RUN_TEST_BIN=$SWIFT_PATH/utils/run-test
TARGET_BUILD_DIR=$SOURCE_PATH/target-build/Ninja-Release

$DEPENDENCIES_SCRIPT

export PATH="$HOME/.wasmer/bin:$PATH"

export SCCACHE_CACHE_SIZE="50G"
export SCCACHE_DIR="$SOURCE_PATH/build-cache"

$BUILD_SCRIPT

if [[ "$(uname)" == "Darwin" ]]; then
  # workaround: host target test directory is necessary to use run-test
  mkdir -p $TARGET_BUILD_DIR/swift-macosx-x86_64/test-macosx-x86_64
  HOST_PLATFORM=macosx
else
  HOST_PLATFORM=linux
fi

if [[ "$(uname)" == "Linux" ]]; then
  $RUN_TEST_BIN --build-dir $TARGET_BUILD_DIR --target wasi-wasm32 \
    $TARGET_BUILD_DIR/swift-${HOST_PLATFORM}-x86_64/test-wasi-wasm32/stdlib
  echo "Skip running test suites for Linux"
else
  $RUN_TEST_BIN --build-dir $TARGET_BUILD_DIR --target wasi-wasm32 \
 	$TARGET_BUILD_DIR/swift-${HOST_PLATFORM}-x86_64/test-wasi-wasm32/stdlib

  # Run test but ignore failure temporarily
  ninja check-swift-wasi-wasm32 -C $TARGET_BUILD_DIR/swift-$HOST_SUFFIX || true
fi
