// RUN: %target-typecheck-verify-swift -swift-version 6

class NonSendable {} // expected-note 3{{class 'NonSendable' does not conform to the 'Sendable' protocol}}

func callee(_: @Sendable () -> NonSendable) {}

var testLocalCaptures: Int {
  let ns = NonSendable()

  @Sendable func localFunc() -> NonSendable {
    return ns // expected-error {{capture of 'ns' with non-sendable type 'NonSendable' in a `@Sendable` local function}}
  }

  callee { return ns } // expected-error {{capture of 'ns' with non-sendable type 'NonSendable' in a `@Sendable` closure}}

  return 3
}

struct Bad {
  var c: Int = {
    let ns = NonSendable()
    callee { return ns } // expected-error {{capture of 'ns' with non-sendable type 'NonSendable' in a `@Sendable` closure}}
    return 3
  }()
}

do {
  class Test { // expected-note 2 {{class 'Test' does not conform to the 'Sendable' protocol}}
    func update() {}
  }

  func sendable(_: @Sendable () -> Void) {}

  @preconcurrency
  func sendable_preconcurrency(_: @Sendable () -> Void) {}

  func withMutable(_: (inout Test) -> Void) {}

  withMutable { test in
    sendable {
      test.update()
      // expected-error@-1 {{capture of 'test' with non-sendable type 'Test' in a `@Sendable` closure}}
      // expected-error@-2 {{mutable capture of 'inout' parameter 'test' is not allowed in concurrently-executing code}}
    }

    sendable_preconcurrency {
      test.update()
      // expected-error@-1 {{capture of 'test' with non-sendable type 'Test' in a `@Sendable` closure}}
      // expected-error@-2 {{mutable capture of 'inout' parameter 'test' is not allowed in concurrently-executing code}}
    }
  }
}
