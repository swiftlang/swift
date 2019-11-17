#/bin/bash

export sourcedir=$PWD/..

./utils/build-script --release --wasm --verbose \
  --skip-build-benchmarks \
  --extra-cmake-options=" \
    -DSWIFT_PRIMARY_VARIANT_SDK:STRING=WASM \
    -DSWIFT_PRIMARY_VARIANT_ARCH:STRING=wasm32 \
    -DSWIFT_OSX_x86_64_ICU_STATICLIB=TRUE \
    -DSWIFT_BUILD_SOURCEKIT=FALSE \
    -DSWIFT_ENABLE_SOURCEKIT_TESTS=FALSE \
    -DCMAKE_AR='/usr/local/opt/llvm/bin/llvm-ar' \
    -DCMAKE_RANLIB='/usr/local/opt/llvm/bin/llvm-ranlib' \
  " \
  --build-stdlib-deployment-targets "wasm-wasm32" \
  --build-swift-dynamic-sdk-overlay false \
  --build-swift-static-sdk-overlay false \
  --build-swift-static-stdlib \
  --llvm-targets-to-build "X86;WebAssembly" \
  --stdlib-deployment-targets "wasm-wasm32" \
  --wasm-icu-data "todo-icu-data" \
  --wasm-icu-i18n "$sourcedir/icu_out/lib" \
  --wasm-icu-i18n-include "$sourcedir/icu_out/include" \
  --wasm-icu-uc "$sourcedir/icu_out/lib" \
  --wasm-icu-uc-include "$sourcedir/icu_out/include" \
  --wasm-wasi-sdk "$sourcedir/wasi-sdk" \
  --install-swift \
  --install-prefix="/opt/swiftwasm-sdk" \
  --install-destdir="$sourcedir/install" \
  --installable-package="$sourcedir/swiftwasm-macos.tar.gz"
