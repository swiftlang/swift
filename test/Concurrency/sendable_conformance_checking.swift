// RUN: %target-swift-frontend -emit-sil %s -o /dev/null -verify
// RUN: %target-swift-frontend -emit-sil %s -o /dev/null -verify -strict-concurrency=targeted
// RUN: %target-swift-frontend -emit-sil %s -o /dev/null -verify -verify-additional-prefix complete-and-tns- -strict-concurrency=complete
// RUN: %target-swift-frontend -emit-sil %s -o /dev/null -verify -verify-additional-prefix complete-and-tns- -strict-concurrency=complete -enable-upcoming-feature RegionBasedIsolation

// REQUIRES: concurrency
// REQUIRES: swift_feature_RegionBasedIsolation

@available(SwiftStdlib 5.1, *)
class NotSendable { // expected-note 9{{class 'NotSendable' does not conform to the 'Sendable' protocol}}
}

@available(SwiftStdlib 5.1, *)
@available(*, unavailable)
extension NotSendable: Sendable { }

@available(SwiftStdlib 5.1, *)
protocol IsolatedWithNotSendableRequirements: Actor {
  func f() -> NotSendable
  var prop: NotSendable { get }

  func fAsync() async -> NotSendable
}

// Okay, everything is isolated the same way
@available(SwiftStdlib 5.1, *)
actor A1: IsolatedWithNotSendableRequirements {
  func f() -> NotSendable { NotSendable() }
  var prop: NotSendable { NotSendable() }
  func fAsync() async -> NotSendable { NotSendable() }
}

// Okay, sendable checking occurs when calling through the protocol
// and also inside the bodies.
@available(SwiftStdlib 5.1, *)
actor A2: IsolatedWithNotSendableRequirements {
  nonisolated func f() -> NotSendable { NotSendable() }
  nonisolated var prop: NotSendable { NotSendable() }

  nonisolated func fAsync() async -> NotSendable { NotSendable() }
  // expected-warning@-1{{non-Sendable type 'NotSendable' cannot be returned from nonisolated implementation to caller of protocol requirement 'fAsync()'}}
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
  func f() async -> NotSendable { NotSendable() } // expected-warning{{non-Sendable type 'NotSendable' cannot be returned from actor-isolated implementation to caller of protocol requirement 'f()'}}

  var prop: NotSendable { // expected-warning{{non-Sendable type 'NotSendable' cannot be returned from actor-isolated implementation to caller of protocol requirement 'prop'}}
    get async {
      NotSendable()
    }
  }
}

// Sendable checking required because calls through protocol cross into the
// actor's domain.
@available(SwiftStdlib 5.1, *)
actor A4: AsyncProtocolWithNotSendable {
  func f() -> NotSendable { NotSendable() } // expected-warning{{non-Sendable type 'NotSendable' cannot be returned from actor-isolated implementation to caller of protocol requirement 'f()'}}

  var prop: NotSendable { // expected-warning{{non-Sendable type 'NotSendable' cannot be returned from actor-isolated implementation to caller of protocol requirement 'prop'}}
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
  func f() async -> NotSendable { NotSendable() } // expected-warning{{non-Sendable type 'NotSendable' cannot be returned from actor-isolated implementation to caller of protocol requirement 'f()'}}

  var prop: NotSendable { // expected-warning{{non-Sendable type 'NotSendable' cannot be returned from actor-isolated implementation to caller of protocol requirement 'prop'}}
    get async {
      NotSendable()
    }
  }
}

// Sendable checking required because calls through protocol cross into the
// actor's domain.
@available(SwiftStdlib 5.1, *)
actor A8: AsyncThrowingProtocolWithNotSendable {
  func f() -> NotSendable { NotSendable() } // expected-warning{{non-Sendable type 'NotSendable' cannot be returned from actor-isolated implementation to caller of protocol requirement 'f()'}}

  var prop: NotSendable { // expected-warning{{non-Sendable type 'NotSendable' cannot be returned from actor-isolated implementation to caller of protocol requirement 'prop'}}
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

// rdar://86653457 - Crash due to missing Sendable conformances.
// expected-warning @+1 {{non-final class 'Klass' can not conform to the 'Sendable' protocol; this is an error in the Swift 6 language mode}}
class Klass<Output: Sendable>: Sendable {}
// expected-complete-and-tns-warning @+1 {{type 'S' does not conform to the 'Sendable' protocol}}
final class SubKlass: Klass<[S]> {}
// expected-complete-and-tns-note @+1 {{consider making struct 'S' conform to the 'Sendable' protocol}}
public struct S {}

// rdar://88700507 - redundant conformance of @MainActor-isolated subclass to 'Sendable'
@available(SwiftStdlib 5.1, *)
@MainActor class MainSuper {}

@available(SwiftStdlib 5.1, *)
class MainSub: MainSuper, @unchecked Sendable {}

class SendableSuper: @unchecked Sendable {}
class SendableSub: SendableSuper, @unchecked Sendable {}

class SendableExtSub: SendableSuper {}
extension SendableExtSub: @unchecked Sendable {}

// Still want to know about same-class redundancy
class MultiConformance: @unchecked Sendable {} // expected-note {{'MultiConformance' declares conformance to protocol 'Sendable' here}}
extension MultiConformance: @unchecked Sendable {} // expected-warning {{redundant conformance of 'MultiConformance' to protocol 'Sendable'}}

@available(SwiftStdlib 5.1, *)
actor MyActor {
  // expected-warning@+1 {{non-final class 'Nested' can not conform to the 'Sendable' protocol; this is an error in the Swift 6 language mode}}
  class Nested: Sendable {}
}

@Sendable func globalFn() {}

protocol NoSendableReqs {
  var prop: () -> Void { get }
  static var staticProp: () -> Void { get }
}

public struct TestSendableWitnesses1 : NoSendableReqs {
  var prop: @Sendable () -> Void // Ok (no warnings)
  static let staticProp: @Sendable () -> Void = { } // Ok (no warnings)
}

public struct TestSendableWitnesses2 : NoSendableReqs {
  var prop = globalFn // Ok (no warnings)
  static let staticProp = globalFn // Ok (no warnings)
}

// @preconcurrency attributes to make it akin to an imported Obj-C API
@preconcurrency @MainActor public protocol EscapingSendableProtocol {
  // expected-complete-and-tns-note @+1 {{protocol requires function 'f(handler:)' with type '(@escaping @MainActor @Sendable (Int) -> Void) -> ()'}}
  @preconcurrency func f(handler: @escaping @MainActor @Sendable (Int) -> Void)
}

// TODO: The following error should actually be a warning.
// expected-complete-and-tns-error @+2 {{type 'TestEscapingOnly' does not conform to protocol 'EscapingSendableProtocol'}}
// expected-complete-and-tns-note @+1 {{add stubs for conformance}}
class TestEscapingOnly: EscapingSendableProtocol {
    // expected-complete-and-tns-note @+1 {{candidate has non-matching type '(@escaping (Int) -> Void) -> ()'}}
    func f(handler: @escaping (Int) -> Void) {}
}
