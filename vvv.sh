utils/build-script --release-debuginfo --wasm \
	--llvm-targets-to-build "X86;ARM;AArch64;PowerPC;SystemZ;WebAssembly" \
	--llvm-max-parallel-lto-link-jobs 1 --swift-tools-max-parallel-lto-link-jobs 1 \
	--wasm-emscripten "/home/zhuowei/Documents/emsdk/emscripten/1.38.30" \
	--wasm-icu-uc "todo" \
	--wasm-icu-uc-include "$PWD/NO" \
	--wasm-icu-i18n "todo" \
	--wasm-icu-i18n-include "todo" \
	--wasm-icu-data "todo" \
	"$@"
