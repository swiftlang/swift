// RUN: %target-swift-emit-silgen-ossa(mock-sdk: %clang-importer-sdk) -o /dev/null -enable-sil-opaque-values %s -enable-objc-interop -import-objc-header %S/Inputs/objc_bridged_generic_conformance.h -verify
// RUN: %target-swift-emit-silgen(mock-sdk: %clang-importer-sdk) %s -enable-objc-interop -import-objc-header %S/Inputs/objc_bridged_generic_conformance.h -verify

protocol P { func test() }

extension Thingy: P {
  @objc func test() {}
}
