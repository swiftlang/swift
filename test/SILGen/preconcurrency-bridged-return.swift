// RUN: %target-swift-emit-silgen-ossa(mock-sdk: %clang-importer-sdk) -o /dev/null -enable-sil-opaque-values -import-objc-header %S/Inputs/preconcurrency-bridged-return.h -swift-version 5 %s
// RUN: %target-swift-emit-silgen(mock-sdk: %clang-importer-sdk) -import-objc-header %S/Inputs/preconcurrency-bridged-return.h -swift-version 5 %s
//
// REQUIRES: objc_interop

func test(x: TestClass) {
	let foo = x.foo()
}

