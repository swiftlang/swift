// RUN: %target-swift-emit-silgen(mock-sdk: %clang-importer-sdk) %s -import-objc-header %S/Inputs/objc_bridged_generic_conformance.h -verify -enable-sil-ownership
// REQUIRES: objc_interop

protocol P { func test() }

extension Thingy: P {
  func test() {}
}
