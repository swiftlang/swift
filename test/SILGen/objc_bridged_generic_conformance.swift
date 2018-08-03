// RUN: %target-swift-emit-silgen(mock-sdk: %clang-importer-sdk) %s -enable-objc-interop -import-objc-header %S/Inputs/objc_bridged_generic_conformance.h -verify -enable-sil-ownership

protocol P { func test() }

extension Thingy: P {
  @objc func test() {}
}
