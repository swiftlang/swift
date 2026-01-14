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

  @_borrowed @_owned var three: String { // expected-error {{property cannot be '@_borrowed' and '@_owned' at the same time}}
    get { "" }
  }
  @_owned var four: String { // expected-error {{property must define a 'get' to support '@_owned'}}
    _read {
      let x = ""
      yield x
    }
  }
}

#if hasAttribute(_owned)
#else
#error("hasAttribute should be true!")
#endif
