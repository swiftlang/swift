// RUN: %target-typecheck-verify-swift -swift-version 5 -strict-concurrency=complete
// RUN: %target-swift-emit-silgen %s -swift-version 5 -strict-concurrency=complete

protocol P: Sendable {}
protocol Q: Sendable {}

struct One<T> {  // expected-note {{consider making generic parameter 'T' conform to the 'Sendable' protocol}}
  var t: T  // expected-warning {{stored property 't' of 'Sendable'-conforming generic struct 'One' has non-Sendable type 'T'; this is an error in the Swift 6 language mode}}
}

extension One: P where T: P {}

struct Both<T> {  // expected-note {{consider making generic parameter 'T' conform to the 'Sendable' protocol}}
  var t: T  // expected-warning {{stored property 't' of 'Sendable'-conforming generic struct 'Both' has non-Sendable type 'T'; this is an error in the Swift 6 language mode}}
}

extension Both: P where T: P {}
extension Both: Q where T: Q {}

func takesSendable<T: Sendable>(_: T) {}

takesSendable(One<Int>(t: 3))
takesSendable(Both<Int>(t: 3))
