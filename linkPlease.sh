#!/bin/sh
# Copy to ../build/Ninja-RelWithDebInfoAssert/swift-linux-x86_64/bin
exec /home/zhuowei/wasi-sdk/bin/wasm-ld --error-limit=0 -o hello.wasm \
	/home/zhuowei/wasi-sdk/share/sysroot/lib/wasm32-wasi/crt1.o \
	hello.o \
	-L../lib/swift_static/wasm/wasm32 \
	-L../lib/swift/wasm/wasm32 \
	-L/home/zhuowei/wasi-sdk/share/sysroot/lib/wasm32-wasi \
	-L/home/zhuowei/Documents/BuildICU/icu_out/lib \
	-lswiftCore \
	-lc -lc++ -lc++abi -lswiftImageInspectionShared \
	-licuuc -licudata \
	/home/zhuowei/wasi-sdk/lib/clang/8.0.0/lib/wasi/libclang_rt.builtins-wasm32.a /home/zhuowei/Documents/FakePthread/*.o --verbose
