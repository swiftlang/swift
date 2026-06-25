// RUN: %target-swift-frontend -emit-sil %s -o /dev/null -verify -strict-concurrency=targeted -swift-version 5
// RUN: %target-swift-frontend -emit-sil %s -o /dev/null -verify -verify-additional-prefix swift6- -swift-version 6

// REQUIRES: concurrency
// REQUIRES: objc_interop

// Tests for implicit Sendable conformance with inherited global actor
// isolation and non-Sendable superclasses.

import Foundation

// MARK: - Inherited isolation, no explicit Sendable

// When a class inherits global-actor isolation from its superclass (rather
// than declaring it explicitly), the implicit Sendable conformance is
// maintained for source compatibility. Starting in Swift 6, a warning
// explains where the conformance came from and offers fix-its.

public class NonSendableSuperclass {}

@MainActor
public class IsolatedNonSendable: NonSendableSuperclass {}

// Inherits @MainActor from the superclass. The implicit Sendable conformance
// is kept but diagnosed starting in Swift 6.
public final class InheritedIsolationSubclass: IsolatedNonSendable {}
// expected-swift6-warning @-1 {{class 'InheritedIsolationSubclass' is implicitly 'Sendable' due to inherited 'MainActor' isolation but has a non-Sendable superclass; this will be an error in a future Swift language mode}}
// expected-swift6-note @-2 {{make 'InheritedIsolationSubclass' explicitly 'MainActor' to retain implicit 'Sendable' conformance}} {{1-1=@MainActor }}
// expected-swift6-note @-3 {{mark 'InheritedIsolationSubclass' as '~Sendable' to suppress the implicit conformance}} {{67-67=, ~Sendable}}

// The subclass is still Sendable (source compatibility), so this compiles.
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
