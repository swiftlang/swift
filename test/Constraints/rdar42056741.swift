// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify %s
// REQUIRES: objc_interop

import Foundation

class A {
  static var `default` = A()

  func foo(arg: String) -> Bool {
    return false
  }

  func foo(arg: String, _ flag: UnsafeMutablePointer<ObjCBool>?) -> Bool {
    return true
  }
}

class B {
  var bar: Bool = false
  func baz() {
    bar = A.default.foo(arg: self.) // expected-cparser-error {{expected member name following '.'}} expected-astgen-error {{expected name in member access}}
  }
}
