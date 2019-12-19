#/bin/bash

export sourcedir=$PWD/..

./utils/build-script --release --wasm --verbose \
  --skip-build-benchmarks \
  --extra-cmake-options=" \
    -DSWIFT_SDKS='WASI;LINUX' \
    -DSWIFT_BUILD_SOURCEKIT=FALSE \
    -DSWIFT_ENABLE_SOURCEKIT_TESTS=FALSE \
    -DCMAKE_AR='$sourcedir/wasi-sdk/bin/llvm-ar' \
    -DCMAKE_RANLIB='$sourcedir/wasi-sdk/bin/llvm-ranlib' \
  " \
  --build-stdlib-deployment-targets "wasi-wasm32" \
  --build-swift-dynamic-sdk-overlay false \
  --build-swift-dynamic-stdlib false \
  --build-swift-static-sdk-overlay \
  --build-swift-static-stdlib \
  --install-destdir="$sourcedir/install" \
  --install-prefix="/opt/swiftwasm-sdk" \
  --install-swift \
  --installable-package="$sourcedir/swiftwasm-linux.tar.gz" \
  --llvm-targets-to-build "X86;WebAssembly" \
  --stdlib-deployment-targets "wasi-wasm32" \
  --wasi-icu-data "todo-icu-data" \
  --wasi-icu-i18n "$sourcedir/icu_out/lib" \
  --wasi-icu-i18n-include "$sourcedir/icu_out/include" \
  --wasi-icu-uc "$sourcedir/icu_out/lib" \
  --wasi-icu-uc-include "$sourcedir/icu_out/include" \
  --wasi-sdk "$sourcedir/wasi-sdk"
