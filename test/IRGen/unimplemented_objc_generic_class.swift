// RUN: %target-swift-frontend %s -emit-ir -verify -disable-objc-attr-requires-foundation-module

// REQUIRES: objc_interop

@objc class ObjCBox<T> {
  var x: T // expected-error{{unimplemented}}

  init(x: T) {
    self.x = x
  }
}
