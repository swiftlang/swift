utils/build-script --release-debuginfo --wasm \
	--llvm-targets-to-build "X86;ARM;AArch64;PowerPC;SystemZ;WebAssembly" \
	--llvm-max-parallel-lto-link-jobs 1 --swift-tools-max-parallel-lto-link-jobs 1 \
	--wasm-wasi-sdk "/home/zhuowei/wasi-sdk" \
	--wasm-icu-uc "todo" \
	--wasm-icu-uc-include "/home/zhuowei/Documents/BuildICU/icu_out/include" \
	--wasm-icu-i18n "todo" \
	--wasm-icu-i18n-include "todo" \
	--wasm-icu-data "todo" \
	--build-swift-static-stdlib \
	"$@"
