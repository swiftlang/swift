// RUN: %target-swift-emit-sil -verify %s -o /dev/null
// REQUIRES: objc_interop

// https://github.com/swiftlang/swift/issues/88991
// On the @objc/NSObject path, using an uninitialized stored property as an
// argument to self.init produces a distinct "property access" diagnostic.
import Foundation

class Retained {}
struct NC: ~Copyable {
  let retained = Retained()
}

@objc class SelfInitArgument: NSObject {
  let value: Retained
  let ncValue: NC?
  init(_ value: Retained) {
    self.value = value
    self.ncValue = nil
  }

  init(withNC nc: consuming NC?) {
    self.value = Retained()
    self.ncValue = nc
  }

  override convenience init() {
    self.init(value) // expected-error {{'self' used in property access 'value' before 'self.init' call}}
  }

  convenience init(nonOverride: ()) {
    self.init(withNC: ncValue) // expected-error {{'self' used before 'self.init' call or assignment to 'self'}}
  }

  convenience init?(failable1: ()) {
    self.init(withNC: ncValue) // expected-error {{'self' used before 'self.init' call or assignment to 'self'}}
  }
}
