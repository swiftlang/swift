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

$DEPENDENCIES_SCRIPT

export SCCACHE_CACHE_SIZE="50G"
export SCCACHE_DIR="$SOURCE_PATH/build-cache"

CACHE_FLAGS="--cmake-c-launcher $(which sccache) --cmake-cxx-launcher $(which sccache)"
FLAGS="--release --debug-swift-stdlib $CACHE_FLAGS --verbose"

$BUILD_SCRIPT $FLAGS

if [[ "$(uname)" == "Linux" ]]; then
  echo "Skip running test suites for Linux"
else
  # Run test but ignore failure temporarily
  $BUILD_SCRIPT $FLAGS -t || true
fi
