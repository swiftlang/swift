// RUN: %target-typecheck-verify-swift -swift-version 5
// RUN: %target-typecheck-verify-swift -swift-version 5 -strict-concurrency=complete -verify-additional-prefix complete-

class NonSendable {} // expected-complete-note 3{{class 'NonSendable' does not conform to the 'Sendable' protocol}}

func callee(_: @Sendable () -> NonSendable) {}

var testLocalCaptures: Int {
  let ns = NonSendable()

  @Sendable func localFunc() -> NonSendable {
    return ns // expected-complete-warning {{capture of 'ns' with non-sendable type 'NonSendable' in a `@Sendable` local function}}
  }

  callee { return ns } // expected-complete-warning {{capture of 'ns' with non-sendable type 'NonSendable' in a `@Sendable` closure}}

  return 3
}

struct Bad {
  var c: Int = {
    let ns = NonSendable()
    callee { return ns } // expected-complete-warning {{capture of 'ns' with non-sendable type 'NonSendable' in a `@Sendable` closure}}
    return 3
  }()
}

do {
  class Test { // expected-complete-note 2 {{class 'Test' does not conform to the 'Sendable' protocol}}
    func update() {}
  }

  func sendable(_: @Sendable () -> Void) {}

  @preconcurrency
  func sendable_preconcurrency(_: @Sendable () -> Void) {}
  
  func withMutable(_: (inout Test) -> Void) {}

  withMutable { test in
    sendable {
      test.update()
      // expected-complete-warning@-1 {{capture of 'test' with non-sendable type 'Test' in a `@Sendable` closure}}
      // expected-warning@-2 {{mutable capture of 'inout' parameter 'test' is not allowed in concurrently-executing code}}
    }

    sendable_preconcurrency {
      test.update()
      // expected-complete-warning@-1 {{capture of 'test' with non-sendable type 'Test' in a `@Sendable` closure}}
      // expected-complete-warning@-2 {{mutable capture of 'inout' parameter 'test' is not allowed in concurrently-executing code}}
    }
  }
}
