// RUN: %target-typecheck-verify-swift -Xcc -fmodule-map-file=%S/Inputs/objc_implementation_private.modulemap -enable-experimental-feature ObjCImplementation -target %target-stable-abi-triple -debug-diagnostic-names
// REQUIRES: objc_interop
// REQUIRES: swift_feature_ObjCImplementation

import objc_implementation_internal

@available(*, unavailable)
@objc @implementation extension ObjCPropertyTest {
  // FIXME: Shouldn't this be on the `@available` above?
  // expected-note@+1 {{'prop1' has been explicitly marked unavailable here}}
  let prop1: Int32

  // expected-note@+1 2 {{'prop2' has been explicitly marked unavailable here}}
  var prop2: Int32 {
    didSet {
      _ = prop2 // expected-error {{'prop2' is unavailable}}
    }
  }

  override init() {
    self.prop1 = 1 // expected-error {{'prop1' is unavailable}}
    self.prop2 = 2 // expected-error {{'prop2' is unavailable}}
    super.init()
  }

  func doSomething() {
    _ = self.prop1
    _ = self.prop2
  }
}
