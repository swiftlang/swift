// RUN: %target-typecheck-verify-swift -swift-version 6

class NonSendable {} // expected-note 2{{class 'NonSendable' does not conform to the 'Sendable' protocol}}

var testLocalCaptures: Int {
  let ns = NonSendable()

  @Sendable func localFunc() -> NonSendable {
    return ns // expected-error {{capture of 'ns' with non-sendable type 'NonSendable' in a `@Sendable` local function}}
  }

  let _: @Sendable () -> NonSendable = {
    return ns // expected-error {{capture of 'ns' with non-sendable type 'NonSendable' in a `@Sendable` closure}}
  }

  return 3
}