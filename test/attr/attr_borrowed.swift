// RUN: %target-typecheck-verify-swift
// REQUIRES: objc_interop

import Foundation

@_borrowed // expected-error {{'@_borrowed' attribute cannot be applied to this declaration}}
func foo() -> String {}

@_borrowed
var string = ""

@objc protocol P {
  @_borrowed // expected-error {{property cannot be '@_borrowed' if it is an @objc protocol requirement}}
  var title: String { get }
}

@objc class A {
  @_borrowed // expected-error {{property cannot be '@_borrowed' if it is '@objc dynamic'}}
  @objc dynamic var title: String { return "" }
}
