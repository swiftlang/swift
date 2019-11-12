#/bin/bash

utils/build-script --release --wasm --verbose \
  --skip-build-benchmarks \
  --extra-cmake-options=" \
    -DSWIFT_SDKS='WASM;LINUX' \
    -DSWIFT_BUILD_SOURCEKIT=FALSE \
    -DSWIFT_ENABLE_SOURCEKIT_TESTS=FALSE \
    -DCMAKE_AR='$sourcedir/wasi-sdk/bin/llvm-ar' \
    -DCMAKE_RANLIB='$sourcedir/wasi-sdk/bin/llvm-ranlib' \
  " \
  --build-stdlib-deployment-targets "wasm-wasm32" \
  --build-swift-static-stdlib \
  --install-destdir="$sourcedir/install" \
  --install-prefix="/opt/swiftwasm-sdk" \
  --install-swift \
  --installable-package="$sourcedir/swiftwasm.tar.gz" \
  --llvm-targets-to-build "X86;WebAssembly" \
  --stdlib-deployment-targets "wasm-wasm32" \
  --wasm-icu-data "todo-icu-data" \
  --wasm-icu-i18n "$sourcedir/icu_out/lib" \
  --wasm-icu-i18n-include "$sourcedir/icu_out/include" \
  --wasm-icu-uc "$sourcedir/icu_out/lib" \
  --wasm-icu-uc-include "$sourcedir/icu_out/include" \
  --wasm-wasi-sdk "$sourcedir/wasi-sdk"
