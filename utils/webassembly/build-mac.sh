#/bin/bash

SOURCE_PATH="$( cd "$(dirname $0)/../../.." && pwd )"
SWIFT_PATH=$SOURCE_PATH/swift

$SWIFT_PATH/utils/build-script --wasm \
  --skip-build-benchmarks \
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
  --build-stdlib-deployment-targets "wasi-wasm32" \
  --build-swift-dynamic-sdk-overlay false \
  --build-swift-dynamic-stdlib false \
  --build-swift-static-sdk-overlay \
  --build-swift-static-stdlib \
  --llvm-targets-to-build "X86;WebAssembly" \
  --stdlib-deployment-targets "wasi-wasm32" \
  --wasi-icu-data "$SOURCE_PATH/icu_out/lib/libicudata.a" \
  --wasi-icu-i18n "$SOURCE_PATH/icu_out/lib/libicui18n.a" \
  --wasi-icu-i18n-include "$SOURCE_PATH/icu_out/include" \
  --wasi-icu-uc "$SOURCE_PATH/icu_out/lib/libicuuc.a" \
  --wasi-icu-uc-include "$SOURCE_PATH/icu_out/include" \
  --wasi-sdk "$SOURCE_PATH/wasi-sdk" \
  --install-swift \
  --install-prefix="/opt/swiftwasm-sdk" \
  --install-destdir="$SOURCE_PATH/install" \
  --installable-package="$SOURCE_PATH/swiftwasm-macos.tar.gz" \
  "$@"
