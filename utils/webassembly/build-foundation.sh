#!/bin/bash
set -ex
DESTINATION_TOOLCHAIN=$1
SOURCE_PATH="$(cd "$(dirname $0)/../../.." && pwd)"

# Remove host CoreFoundation (which can be different from the target) headers
# to avoid shadowing the wasm32 target CoreFoundation
rm -rf $DESTINATION_TOOLCHAIN/usr/lib/swift/CoreFoundation

FOUNDATION_BUILD="$SOURCE_PATH/build/Ninja-ReleaseAssert/foundation-wasi-wasm32"

mkdir -p $FOUNDATION_BUILD
cd $FOUNDATION_BUILD

cmake -G Ninja \
  -DCMAKE_Swift_COMPILER="$DESTINATION_TOOLCHAIN/usr/bin/swiftc" \
  -DCMAKE_STAGING_PREFIX="$DESTINATION_TOOLCHAIN/usr" \
  -DCMAKE_TOOLCHAIN_FILE="$SOURCE_PATH/swift/utils/webassembly/toolchain-wasi.cmake" \
  -DWASI_SDK_PATH="$SOURCE_PATH/wasi-sdk" \
  -DICU_ROOT="$SOURCE_PATH/icu_out" \
  -DBUILD_SHARED_LIBS=OFF \
  "${SOURCE_PATH}/swift-corelibs-foundation"
  
ninja -v
ninja -v install

# On macOS the target CoreFoundation shadows the CoreFoundation suppplied by Xcode.
# On Linux though there's no system CoreFoundation, its headers have to be shipped
# in the installable archive and serve for both host and target.
if [[ "$(uname)" == "Darwin" ]]; then
  mv $DESTINATION_TOOLCHAIN/usr/lib/swift_static/CoreFoundation \
    $DESTINATION_TOOLCHAIN/usr/lib/swift/wasi/wasm32/CoreFoundation
fi

# .swiftdoc and .swiftmodule files should live in `swift`, not in `swift_static`
mv $DESTINATION_TOOLCHAIN/usr/lib/swift_static/wasi/wasm32/Foundation.swift* \
  $DESTINATION_TOOLCHAIN/usr/lib/swift/wasi/wasm32