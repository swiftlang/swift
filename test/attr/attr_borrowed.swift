// RUN: %target-typecheck-verify-swift -target %target-swift-5.1-abi-triple
// REQUIRES: objc_interop
// REQUIRES: concurrency

import Foundation

@_borrowed // expected-error {{'@_borrowed' attribute cannot be applied to this declaration}}
func foo() -> String {}

@_borrowed
var string = ""

@objc protocol P {
  @_borrowed // expected-error {{property cannot be '@_borrowed' if it is an '@objc' protocol requirement}}
  var title: String { get }
}

@objc class A {
  @_borrowed // expected-error {{property cannot be '@_borrowed' if it is '@objc dynamic'}}
  @objc dynamic var title: String { return "" }
}

public class Holder {
  @_borrowed var one: String {
    get async { "" } // expected-error {{getter cannot be '@_borrowed' if it is 'async' or 'throws'}}
  }
  @_borrowed var two: String {
    get throws { "" } // expected-error {{getter cannot be '@_borrowed' if it is 'async' or 'throws'}}
  }
}
