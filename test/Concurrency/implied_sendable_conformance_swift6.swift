// RUN: %target-typecheck-verify-swift -swift-version 6

protocol P: Sendable {}
protocol Q: Sendable {}

struct One<T> {  // expected-note {{consider making generic parameter 'T' conform to the 'Sendable' protocol}}
  var t: T  // expected-error {{stored property 't' of 'Sendable'-conforming generic struct 'One' has non-sendable type 'T'}}
}

extension One: P where T: P {}
// expected-error@-1 {{conditional conformance of type 'One<T>' to protocol 'P' does not imply conformance to inherited protocol 'Sendable'}}
// expected-note@-2 {{did you mean to explicitly state the conformance like 'extension One: Sendable where ...'}}

struct Both<T> { // expected-note {{consider making generic parameter 'T' conform to the 'Sendable' protocol}}
  var t: T  // expected-error {{stored property 't' of 'Sendable'-conforming generic struct 'Both' has non-sendable type 'T'}}
}

extension Both: P where T: P {}
// expected-error@-1 {{conditional conformance of type 'Both<T>' to protocol 'P' does not imply conformance to inherited protocol 'Sendable'}}
// expected-note@-2 {{did you mean to explicitly state the conformance like 'extension Both: Sendable where ...'}}

extension Both: Q where T: Q {}

func takesSendable<T: Sendable>(_: T) {}

takesSendable(One<Int>(t: 3))
takesSendable(Both<Int>(t: 3))
