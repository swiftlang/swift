// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-silgen %s -import-objc-header %S/Inputs/objc_bridged_generic_conformance.h -verify
// REQUIRES: objc_interop

protocol P { func test() }

extension Thingy: P {
  func test() {}
}
