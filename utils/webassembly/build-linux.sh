#/bin/bash

SOURCE_PATH="$( cd "$(dirname $0)/../../.." && pwd )"
SWIFT_PATH=$SOURCE_PATH/swift

$SWIFT_PATH/utils/build-script --wasm \
  --extra-cmake-options=" \
    -DSWIFT_PRIMARY_VARIANT_SDK:STRING=WASI \
    -DSWIFT_PRIMARY_VARIANT_ARCH:STRING=wasm32 \
    -DSWIFT_SDKS='WASI;LINUX' \
    -DSWIFT_BUILD_SOURCEKIT=FALSE \
    -DSWIFT_ENABLE_SOURCEKIT_TESTS=FALSE \
    -DSWIFT_BUILD_SYNTAXPARSERLIB=FALSE \
    -DCMAKE_AR='$SOURCE_PATH/wasi-sdk/bin/llvm-ar' \
    -DCMAKE_RANLIB='$SOURCE_PATH/wasi-sdk/bin/llvm-ranlib' \
  " \
  "$@"
