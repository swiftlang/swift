// RUN: %target-typecheck-verify-swift -swift-version 6

protocol P: Sendable {}
protocol Q: Sendable {}

struct One<T> {  // expected-note {{consider making generic parameter 'T' conform to the 'Sendable' protocol}}
  var t: T  // expected-error {{stored property 't' of 'Sendable'-conforming generic struct 'One' has non-Sendable type 'T'}}
}

extension One: P where T: P {}
// expected-error@-1 {{conditional conformance of type 'One<T>' to protocol 'P' does not imply conformance to inherited protocol 'Sendable'}}
// expected-note@-2 {{did you mean to explicitly state the conformance with relaxed bounds using 'where T: Sendable'?}}
// expected-note@-3 {{did you mean to explicitly state the conformance with the same bounds using 'where T: P'?}}
// expected-note@-4 {{did you mean to explicitly state the conformance with different bounds?}}

struct Both<T> { // expected-note {{consider making generic parameter 'T' conform to the 'Sendable' protocol}}
  var t: T  // expected-error {{stored property 't' of 'Sendable'-conforming generic struct 'Both' has non-Sendable type 'T'}}
}

extension Both: P where T: P {}
// expected-error@-1 {{conditional conformance of type 'Both<T>' to protocol 'P' does not imply conformance to inherited protocol 'Sendable'}}
// expected-note@-2 {{did you mean to explicitly state the conformance with relaxed bounds using 'where T: Sendable'?}}
// expected-note@-3 {{did you mean to explicitly state the conformance with the same bounds using 'where T: P'?}}
// expected-note@-4 {{did you mean to explicitly state the conformance with different bounds?}}

extension Both: Q where T: Q {}

func takesSendable<T: Sendable>(_: T) {}

takesSendable(One<Int>(t: 3))
takesSendable(Both<Int>(t: 3))
