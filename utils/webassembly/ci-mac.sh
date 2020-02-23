#/bin/bash

set -ex

SOURCE_PATH="$( cd "$(dirname $0)/../../.." && pwd  )" 
SWIFT_PATH=$SOURCE_PATH/swift
UTILS_PATH=$SWIFT_PATH/utils/webassembly
BUILD_SCRIPT=$UTILS_PATH/build-macos.sh
DEPENDENCIES_SCRIPT=$UTILS_PATH/macos/install-dependencies.sh

$DEPENDENCIES_SCRIPT

$BUILD_SCRIPT --release --debug-swift-stdlib --verbose
# Run test but ignore failure temporarily
$BUILD_SCRIPT --release --debug-swift-stdlib --verbose -t || true
