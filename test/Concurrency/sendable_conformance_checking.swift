// RUN: %target-typecheck-verify-swift
// REQUIRES: concurrency

@available(SwiftStdlib 5.1, *)
class NotSendable { // expected-note 8{{class 'NotSendable' does not conform to the 'Sendable' protocol}}
}

@available(SwiftStdlib 5.1, *)
@available(*, unavailable)
extension NotSendable: Sendable { }

@available(SwiftStdlib 5.1, *)
protocol IsolatedWithNotSendableRequirements: Actor {
  func f() -> NotSendable
  var prop: NotSendable { get }
}

// Okay, everything is isolated the same way
@available(SwiftStdlib 5.1, *)
actor A1: IsolatedWithNotSendableRequirements {
  func f() -> NotSendable { NotSendable() }
  var prop: NotSendable { NotSendable() }
}

// Okay, sendable checking occurs when calling through the protocol
// and also inside the bodies.
@available(SwiftStdlib 5.1, *)
actor A2: IsolatedWithNotSendableRequirements {
  nonisolated func f() -> NotSendable { NotSendable() }
  nonisolated var prop: NotSendable { NotSendable() }
}

@available(SwiftStdlib 5.1, *)
protocol AsyncProtocolWithNotSendable {
  func f() async -> NotSendable
  var prop: NotSendable { get async }
}

// Sendable checking required because calls through protocol cross into the
// actor's domain.
@available(SwiftStdlib 5.1, *)
actor A3: AsyncProtocolWithNotSendable {
  func f() async -> NotSendable { NotSendable() } // expected-warning{{cannot call function returning non-sendable type 'NotSendable' across actors}}

  var prop: NotSendable { // expected-warning{{cannot use property 'prop' with a non-sendable type 'NotSendable' across actors}}
    get async {
      NotSendable()
    }
  }
}

// Sendable checking required because calls through protocol cross into the
// actor's domain.
@available(SwiftStdlib 5.1, *)
actor A4: AsyncProtocolWithNotSendable {
  func f() -> NotSendable { NotSendable() } // expected-warning{{cannot call function returning non-sendable type 'NotSendable' across actors}}

  var prop: NotSendable { // expected-warning{{cannot use property 'prop' with a non-sendable type 'NotSendable' across actors}}
    get {
      NotSendable()
    }
  }
}

// Sendable checking not required because we never cross into the actor's
// domain.
@available(SwiftStdlib 5.1, *)
actor A5: AsyncProtocolWithNotSendable {
  nonisolated func f() async -> NotSendable { NotSendable() }

  nonisolated var prop: NotSendable {
    get async {
      NotSendable()
    }
  }
}

// Sendable checking not required because we never cross into the actor's
// domain.
@available(SwiftStdlib 5.1, *)
actor A6: AsyncProtocolWithNotSendable {
  nonisolated func f() -> NotSendable { NotSendable() }

  nonisolated var prop: NotSendable {
    get {
      NotSendable()
    }
  }
}

@available(SwiftStdlib 5.1, *)
protocol AsyncThrowingProtocolWithNotSendable {
  func f() async throws -> NotSendable
  var prop: NotSendable { get async throws }
}

// Sendable checking required because calls through protocol cross into the
// actor's domain.
@available(SwiftStdlib 5.1, *)
actor A7: AsyncThrowingProtocolWithNotSendable {
  func f() async -> NotSendable { NotSendable() } // expected-warning{{cannot call function returning non-sendable type 'NotSendable' across actors}}

  var prop: NotSendable { // expected-warning{{cannot use property 'prop' with a non-sendable type 'NotSendable' across actors}}
    get async {
      NotSendable()
    }
  }
}

// Sendable checking required because calls through protocol cross into the
// actor's domain.
@available(SwiftStdlib 5.1, *)
actor A8: AsyncThrowingProtocolWithNotSendable {
  func f() -> NotSendable { NotSendable() } // expected-warning{{cannot call function returning non-sendable type 'NotSendable' across actors}}

  var prop: NotSendable { // expected-warning{{cannot use property 'prop' with a non-sendable type 'NotSendable' across actors}}
    get {
      NotSendable()
    }
  }
}

// Sendable checking not required because we never cross into the actor's
// domain.
@available(SwiftStdlib 5.1, *)
actor A9: AsyncThrowingProtocolWithNotSendable {
  nonisolated func f() async -> NotSendable { NotSendable() }

  nonisolated var prop: NotSendable {
    get async {
      NotSendable()
    }
  }
}

// Sendable checking not required because we never cross into the actor's
// domain.
@available(SwiftStdlib 5.1, *)
actor A10: AsyncThrowingProtocolWithNotSendable {
  nonisolated func f() -> NotSendable { NotSendable() }

  nonisolated var prop: NotSendable {
    get {
      NotSendable()
    }
  }
}
