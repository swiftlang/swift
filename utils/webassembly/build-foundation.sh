#!/bin/bash
set -ex
DESTINATION_TOOLCHAIN=$1
WASI_SYSROOT_PATH=$2
SOURCE_PATH="$(cd "$(dirname $0)/../../.." && pwd)"

FOUNDATION_BUILD="$SOURCE_PATH/target-build/foundation-wasi-wasm32"

mkdir -p $FOUNDATION_BUILD
cd $FOUNDATION_BUILD

cmake -G Ninja \
  -DCMAKE_SYSROOT="$WASI_SYSROOT_PATH" \
  -DCMAKE_Swift_COMPILER="$DESTINATION_TOOLCHAIN/usr/bin/swiftc" \
  -DCMAKE_STAGING_PREFIX="$DESTINATION_TOOLCHAIN/usr" \
  -DCMAKE_TOOLCHAIN_FILE="$SOURCE_PATH/swift/utils/webassembly/toolchain-wasi.cmake" \
  -DLLVM_BIN="$DESTINATION_TOOLCHAIN/usr/bin" \
  -DICU_ROOT="$SOURCE_PATH/icu_out" \
  -DBUILD_SHARED_LIBS=OFF \
  -DCMAKE_Swift_COMPILER_FORCED=ON \
  "${SOURCE_PATH}/swift-corelibs-foundation"
  
ninja -v
ninja -v install
