#/bin/bash

SOURCE_PATH="$( cd "$(dirname $0)/../../.." && pwd )"
SWIFT_PATH=$SOURCE_PATH/swift

$SWIFT_PATH/utils/build-script --wasm \
  --extra-cmake-options=" \
    -DSWIFT_PRIMARY_VARIANT_SDK:STRING=WASI \
    -DSWIFT_PRIMARY_VARIANT_ARCH:STRING=wasm32 \
    -DSWIFT_OSX_x86_64_ICU_STATICLIB=TRUE \
    -DSWIFT_BUILD_SOURCEKIT=FALSE \
    -DSWIFT_ENABLE_SOURCEKIT_TESTS=FALSE \
    -DSWIFT_BUILD_SYNTAXPARSERLIB=FALSE \
    -DCMAKE_AR='/usr/local/opt/llvm/bin/llvm-ar' \
    -DCMAKE_RANLIB='/usr/local/opt/llvm/bin/llvm-ranlib' \
  " \
  "$@"
