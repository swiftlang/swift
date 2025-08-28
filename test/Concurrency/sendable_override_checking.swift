// RUN: %target-swift-frontend -verify -emit-sil -o /dev/null %s
// RUN: %target-swift-frontend -verify -emit-sil -o /dev/null %s -strict-concurrency=targeted
// RUN: %target-swift-frontend -verify -emit-sil -o /dev/null %s -strict-concurrency=complete

// REQUIRES: concurrency

@available(SwiftStdlib 5.1, *)
class NotSendable { // expected-note 2{{class 'NotSendable' does not conform to the 'Sendable' protocol}}
}

@available(SwiftStdlib 5.1, *)
@available(*, unavailable)
extension NotSendable: Sendable { }

@available(SwiftStdlib 5.1, *)
class Super {
  func f(_: NotSendable) async { }
  @MainActor func g1(_: NotSendable) { }
  @MainActor func g2(_: NotSendable) async { }
}

@available(SwiftStdlib 5.1, *)
class Sub: Super {
  @MainActor override func f(_: NotSendable) async { }
  // expected-warning@-1{{non-Sendable parameter type 'NotSendable' cannot be sent from caller of superclass instance method 'f' into main actor-isolated override}}

  nonisolated override func g1(_: NotSendable) { } // okay, synchronous

  nonisolated override func g2(_: NotSendable) async { }
  // expected-warning@-1{{non-Sendable parameter type 'NotSendable' cannot be sent from caller of superclass instance method 'g2' into nonisolated override}}
}
