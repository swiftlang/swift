// RUN: %target-swift-emit-silgen-ossa(mock-sdk: %clang-importer-sdk) -o /dev/null -enable-sil-opaque-values -import-objc-header %S/Inputs/preconcurrency-bridged-return.h %s
// RUN: %target-swift-emit-silgen(mock-sdk: %clang-importer-sdk) -import-objc-header %S/Inputs/preconcurrency-bridged-return.h %s
//
// REQUIRES: objc_interop

func test(x: TestClass) {
	let foo = x.foo()
}

