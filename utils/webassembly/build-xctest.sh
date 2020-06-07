#!/bin/bash
set -ex
DESTINATION_TOOLCHAIN=$1
SOURCE_PATH="$(cd "$(dirname $0)/../../.." && pwd)"

BUILD_DIR="$SOURCE_PATH/build/Ninja-ReleaseAssert/xctest-wasi-wasm32"

mkdir -p $BUILD_DIR
cd $BUILD_DIR

cmake -G Ninja \
  -DCMAKE_Swift_COMPILER="$DESTINATION_TOOLCHAIN/usr/bin/swiftc" \
  -DCMAKE_STAGING_PREFIX="$DESTINATION_TOOLCHAIN/usr" \
  -DCMAKE_TOOLCHAIN_FILE="$SOURCE_PATH/swift/utils/webassembly/toolchain-wasi.cmake" \
  -DWASI_SDK_PATH="$SOURCE_PATH/wasi-sdk" \
  -DBUILD_SHARED_LIBS=OFF \
  -DSWIFT_FOUNDATION_PATH=$DESTINATION_TOOLCHAIN/usr/lib/swift/wasi/wasm32 \
  "${SOURCE_PATH}/swift-corelibs-xctest"
  
ninja -v
ninja -v install
