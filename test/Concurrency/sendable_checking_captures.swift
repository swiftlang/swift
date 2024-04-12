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