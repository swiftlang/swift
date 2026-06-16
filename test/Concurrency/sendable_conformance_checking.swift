// RUN: %target-swift-frontend -emit-sil %s -o /dev/null -verify
// RUN: %target-swift-frontend -emit-sil %s -o /dev/null -verify -strict-concurrency=targeted
// RUN: %target-swift-frontend -emit-sil %s -o /dev/null -verify -verify-additional-prefix complete- -strict-concurrency=complete

// REQUIRES: concurrency

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
// expected-warning @+1 {{non-final class 'Klass' cannot conform to the 'Sendable' protocol; this is an error in the Swift 6 language mode}} {{1-1=final }}
class Klass<Output: Sendable>: Sendable {}
// expected-complete-warning @+1 {{type 'S' does not conform to the 'Sendable' protocol}}
final class SubKlass: Klass<[S]> {}
// expected-complete-note @+1 {{consider making struct 'S' conform to the 'Sendable' protocol}}
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

// SE-0434: adding global-actor isolation to a non-Sendable superclass does not
// make a class 'Sendable' and an explicit conformance is diagnosed.
@available(SwiftStdlib 5.1, *)
class NonSendableSuperclass {}

// Non-isolated was always rejected, so its an error in the Swift 6 language mode.
@available(SwiftStdlib 5.1, *)
final class SendableSubclassOfNonSendable: NonSendableSuperclass, Sendable {}
// expected-warning@-1:13 {{class 'SendableSubclassOfNonSendable' cannot conform to the 'Sendable' protocol; this is an error in the Swift 6 language mode}}
// expected-note@-2:13 {{a 'Sendable' class cannot inherit from a non-Sendable class}}
// expected-note@-3:44 {{inherits from non-Sendable class 'NonSendableSuperclass'}}

@available(SwiftStdlib 5.1, *)
@globalActor
actor SomeActor {
  static let shared = SomeActor()
}

// Non-final class rule shouldn't fire.
@available(SwiftStdlib 5.1, *)
@MainActor
class IsolatedSendableSubclass: NonSendableSuperclass, Sendable {
  // expected-warning@-1:7 {{class 'IsolatedSendableSubclass' cannot conform to the 'Sendable' protocol; this will be an error in a future Swift language mode}}
  // expected-note@-2:7 {{a 'Sendable' class cannot inherit from a non-Sendable class}}
  // expected-note@-3:33 {{inherits from non-Sendable class 'NonSendableSuperclass'}}
  var state = NonSendableSuperclass() // not flagged: global-actor isolation protects the storage
}

@available(SwiftStdlib 5.1, *)
@SomeActor
class OtherIsolatedSendableSubclass: NonSendableSuperclass, Sendable {}
// expected-warning@-1:7 {{class 'OtherIsolatedSendableSubclass' cannot conform to the 'Sendable' protocol; this will be an error in a future Swift language mode}}
// expected-note@-2:7 {{a 'Sendable' class cannot inherit from a non-Sendable class}}
// expected-note@-3:38 {{inherits from non-Sendable class 'NonSendableSuperclass'}}

@available(SwiftStdlib 5.1, *)
protocol RefinesSendable: Sendable {}

@available(SwiftStdlib 5.1, *)
@MainActor
class IsolatedImpliedSendableSubclass: NonSendableSuperclass, RefinesSendable {}
// expected-warning@-1:7 {{class 'IsolatedImpliedSendableSubclass' cannot conform to the 'Sendable' protocol; this will be an error in a future Swift language mode}}
// expected-note@-2:7 {{a 'Sendable' class cannot inherit from a non-Sendable class}}
// expected-note@-3:40 {{inherits from non-Sendable class 'NonSendableSuperclass'}}

@available(SwiftStdlib 5.1, *)
@MainActor
final class IsolatedExtensionSendableSubclass: NonSendableSuperclass {}
// expected-note@-1:48 {{inherits from non-Sendable class 'NonSendableSuperclass'}}

@available(SwiftStdlib 5.1, *)
extension IsolatedExtensionSendableSubclass: Sendable {}
// expected-warning@-1:1 {{class 'IsolatedExtensionSendableSubclass' cannot conform to the 'Sendable' protocol; this will be an error in a future Swift language mode}}
// expected-note@-2:1 {{a 'Sendable' class cannot inherit from a non-Sendable class}}

@available(SwiftStdlib 5.1, *)
@MainActor
class IsolatedUncheckedSendableSubclass: NonSendableSuperclass, @unchecked Sendable {}

// An unavailable conformance is the explicit "not Sendable" opt-out; exempt.
@available(SwiftStdlib 5.1, *)
@MainActor
class IsolatedUnavailableSendableSubclass: NonSendableSuperclass {}

@available(SwiftStdlib 5.1, *)
@available(*, unavailable)
extension IsolatedUnavailableSendableSubclass: Sendable {}

// Restating Sendable is allowed!
@available(SwiftStdlib 5.1, *)
@MainActor
final class IsolatedSubclassOfIsolated: MainSuper, Sendable {}

// No explicit conformance: the implicit one is withheld, so it isn't 'Sendable'.
@available(SwiftStdlib 5.1, *)
@MainActor
class IsolatedNonSendableSubclass: NonSendableSuperclass {}
// expected-complete-note@-1:7 {{class 'IsolatedNonSendableSubclass' does not conform to the 'Sendable' protocol}}

@available(SwiftStdlib 5.1, *)
func needsSendable<T: Sendable>(_: T) {}

@available(SwiftStdlib 5.1, *)
func passIsolatedNonSendableSubclass(_ instance: IsolatedNonSendableSubclass) {
  needsSendable(instance)
  // expected-complete-warning@-1:3 {{type 'IsolatedNonSendableSubclass' does not conform to the 'Sendable' protocol}}
}

@available(SwiftStdlib 5.1, *)
class TildeNonSendableSuperclass: ~Sendable {}

@available(SwiftStdlib 5.1, *)
@MainActor
final class IsolatedSubclassOfTildeSuper: TildeNonSendableSuperclass, Sendable {}
// expected-warning@-1:13 {{class 'IsolatedSubclassOfTildeSuper' cannot conform to the 'Sendable' protocol; this will be an error in a future Swift language mode}}
// expected-note@-2:13 {{a 'Sendable' class cannot inherit from a non-Sendable class}}
// expected-note@-3:43 {{inherits from non-Sendable class 'TildeNonSendableSuperclass'}}

// `@MainActor` does not override an explicit `~Sendable` suppression, so an
// isolated base is still non-Sendable and the subclass is diagnosed.
@available(SwiftStdlib 5.1, *)
@MainActor
class IsolatedTildeSuper: ~Sendable {}

@available(SwiftStdlib 5.1, *)
@MainActor
final class IsolatedSubclassOfIsolatedTilde: IsolatedTildeSuper, Sendable {}
// expected-warning@-1:13 {{class 'IsolatedSubclassOfIsolatedTilde' cannot conform to the 'Sendable' protocol; this will be an error in a future Swift language mode}}
// expected-note@-2:13 {{a 'Sendable' class cannot inherit from a non-Sendable class}}
// expected-note@-3:46 {{inherits from non-Sendable class 'IsolatedTildeSuper'}}

@available(SwiftStdlib 5.1, *)
actor MyActor {
  // expected-warning@+1 {{non-final class 'Nested' cannot conform to the 'Sendable' protocol; this is an error in the Swift 6 language mode}} {{3-3=final }}
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
  // expected-complete-note @+1 {{protocol requires function 'f(handler:)' with type '(@escaping @MainActor @Sendable (Int) -> Void) -> ()'}}
  @preconcurrency func f(handler: @escaping @MainActor @Sendable (Int) -> Void)
}

// TODO: The following error should actually be a warning.
// expected-complete-error @+2 {{type 'TestEscapingOnly' does not conform to protocol 'EscapingSendableProtocol'}}
// expected-complete-note @+1 {{add stubs for conformance}}
class TestEscapingOnly: EscapingSendableProtocol {
    // expected-complete-note @+1 {{candidate has non-matching type '(@escaping (Int) -> Void) -> ()'}}
    func f(handler: @escaping (Int) -> Void) {}
}
