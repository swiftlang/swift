// RUN: %target-swift-frontend -emit-sil %s -o /dev/null -verify -strict-concurrency=targeted -swift-version 5
// RUN: %target-swift-frontend -emit-sil %s -o /dev/null -verify -swift-version 6

// REQUIRES: concurrency
// REQUIRES: objc_interop

// Tests for implicit Sendable conformance with inherited global actor
// isolation and non-Sendable superclasses.

import Foundation

// MARK: - Inherited isolation, no explicit Sendable

// When a class inherits global-actor isolation from its superclass (rather
// than declaring it explicitly), the implicit Sendable conformance is
// maintained for source compatibility with no diagnostic. The user never
// asked for Sendable, so diagnosing the non-Sendable superclass would be
// confusing.

public class NonSendableSuperclass {}

@MainActor
public class IsolatedNonSendable: NonSendableSuperclass {}

// Inherits @MainActor from the superclass. The implicit Sendable conformance
// is kept silently -- no diagnostic expected.
public final class InheritedIsolationSubclass: IsolatedNonSendable {}

func requiresSendable<T: Sendable>(_: T.Type) {}
func testInheritedIsolationIsSendable() {
  requiresSendable(InheritedIsolationSubclass.self)
}

// MARK: - NSObject superclass (SE-0434)

// NSObject is Sendable, so @MainActor subclasses of NSObject inherit
// Sendable through the normal inherited-conformance path. Their subclasses
// should also be Sendable with no diagnostic.

@MainActor
class IsolatedNSObjectSubclass: NSObject {}

class InheritedFromIsolatedNSObject: IsolatedNSObjectSubclass {}

func testNSObjectChainIsSendable() {
  requiresSendable(IsolatedNSObjectSubclass.self)
  requiresSendable(InheritedFromIsolatedNSObject.self)
}

// MARK: - Inherited isolation with explicit Sendable (should diagnose)

// When Sendable IS explicitly requested somewhere in the chain, the
// non-Sendable superclass diagnostic should still fire.

protocol RefinesSendable: Sendable {}

// Explicit Sendable on a class with inherited isolation.
final class InheritedIsolationExplicitSendable: IsolatedNonSendable, Sendable {}
// expected-warning @-1 {{class 'InheritedIsolationExplicitSendable' cannot conform to the 'Sendable' protocol; this will be an error in a future Swift language mode}}
// expected-note @-2 {{a 'Sendable' class cannot inherit from a non-Sendable class}}
// expected-note @-3 {{inherits from non-Sendable class 'IsolatedNonSendable'}}

// Sendable implied through a protocol refinement.
@MainActor
final class InheritedIsolationRefinedSendable: NonSendableSuperclass, RefinesSendable {}
// expected-warning @-1 {{class 'InheritedIsolationRefinedSendable' cannot conform to the 'Sendable' protocol; this will be an error in a future Swift language mode}}
// expected-note @-2 {{a 'Sendable' class cannot inherit from a non-Sendable class}}
// expected-note @-3 {{inherits from non-Sendable class 'NonSendableSuperclass'}}
