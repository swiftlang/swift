// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -disable-objc-attr-requires-foundation-module -parse %s -emit-fixits-path %t.remap
// RUN: c-arcmt-test %t.remap | arcmt-test -verify-transformed-files %s.result
import ObjectiveC

// REQUIRES: objc_interop

@objc class Selectors {
  func takeSel(_: Selector) {}
  func mySel() {}
  func test() {
    takeSel("mySel")
    takeSel(Selector("mySel"))
  }
}
