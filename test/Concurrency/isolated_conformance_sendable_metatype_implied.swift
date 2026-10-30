// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -typecheck -verify -target %target-swift-5.1-abi-triple -swift-version 6 -enable-upcoming-feature InferIsolatedConformances -verify-additional-prefix swift6- %t/implied.swift
// RUN: %target-swift-frontend -typecheck -verify -target %target-swift-5.1-abi-triple -swift-version 5 -enable-upcoming-feature InferIsolatedConformances -verify-additional-prefix swift5- %t/implied.swift

// RUN: %target-swift-frontend -typecheck -verify -target %target-swift-5.1-abi-triple -swift-version 6 -default-isolation MainActor -verify-additional-prefix swift6- %t/default_main_actor.swift
// RUN: %target-swift-frontend -typecheck -verify -target %target-swift-5.1-abi-triple -swift-version 5 -default-isolation MainActor -verify-additional-prefix swift5- %t/default_main_actor.swift

// REQUIRES: concurrency
// REQUIRES: swift_feature_InferIsolatedConformances

// A conformance implied by a conformance to a protocol that inherits
// 'SendableMetatype' is not inferred to be isolated.

//--- implied.swift

@globalActor
actor SomeGlobalActor {
  static let shared = SomeGlobalActor()
}

// MARK: - Implied by a 'Sendable' refinement: nonisolated

// The requirement is isolated like its witness.
protocol PIsolatedRequirement {
  @MainActor func f()
}

protocol SendablePIsolatedRequirement: PIsolatedRequirement, Sendable {}

@MainActor
final class CIsolatedRequirement: SendablePIsolatedRequirement {
  func f() {}
}

// An 'async' witness.
protocol PAsync {
  func f() async
}

protocol SendablePAsync: PAsync, Sendable {}

@SomeGlobalActor
final class CAsyncWitness: SendablePAsync {
  func f() async {}
}

// A synchronous witness for a nonisolated requirement is diagnosed.
protocol PSync {
  func f()
}

protocol SendablePSync: PSync, Sendable {}

@SomeGlobalActor
final class CSyncWitness: SendablePSync {
  // expected-swift6-error@-1 {{conformance of 'CSyncWitness' to protocol 'PSync' crosses into global actor 'SomeGlobalActor'-isolated code and can cause data races}}
  // expected-swift5-warning@-2 {{conformance of 'CSyncWitness' to protocol 'PSync' crosses into global actor 'SomeGlobalActor'-isolated code and can cause data races; this is an error in the Swift 6 language mode}}
  // expected-note@-3 {{turn data races into runtime errors with '@preconcurrency'}}
  func f() {}
  // expected-note@-1 {{global actor 'SomeGlobalActor'-isolated instance method 'f()' cannot satisfy nonisolated requirement}}
  // expected-note@-2 {{mark instance method 'f()' 'nonisolated'}}
}

// 'Sendable' two refinements up.
protocol Leaf {
  func leaf()
}

protocol Middle: Leaf {}

protocol Root: Middle, Sendable {}

@MainActor
final class CTwoRefinementsUp: Root {
  // expected-swift6-error@-1 {{conformance of 'CTwoRefinementsUp' to protocol 'Leaf' crosses into main actor-isolated code and can cause data races}}
  // expected-swift5-warning@-2 {{conformance of 'CTwoRefinementsUp' to protocol 'Leaf' crosses into main actor-isolated code and can cause data races; this is an error in the Swift 6 language mode}}
  // expected-note@-3 {{turn data races into runtime errors with '@preconcurrency'}}
  func leaf() {}
  // expected-note@-1 {{main actor-isolated instance method 'leaf()' cannot satisfy nonisolated requirement}}
  // expected-note@-2 {{mark instance method 'leaf()' 'nonisolated'}}
}

// Several implied conformances, with associated types that refer back to the
// conforming type.
protocol SendableRandomAccessCollection: RandomAccessCollection, Sendable {}

@MainActor
struct IsolatedSubscriptCollection: SendableRandomAccessCollection {
  // expected-swift6-error@-1 {{conformance of 'IsolatedSubscriptCollection' to protocol 'RandomAccessCollection' crosses into main actor-isolated code and can cause data races}}
  // expected-swift6-error@-2 {{conformance of 'IsolatedSubscriptCollection' to protocol 'BidirectionalCollection' crosses into main actor-isolated code and can cause data races}}
  // expected-swift6-error@-3 {{conformance of 'IsolatedSubscriptCollection' to protocol 'Collection' crosses into main actor-isolated code and can cause data races}}
  // expected-swift5-warning@-4 {{conformance of 'IsolatedSubscriptCollection' to protocol 'RandomAccessCollection' crosses into main actor-isolated code and can cause data races; this is an error in the Swift 6 language mode}}
  // expected-swift5-warning@-5 {{conformance of 'IsolatedSubscriptCollection' to protocol 'BidirectionalCollection' crosses into main actor-isolated code and can cause data races; this is an error in the Swift 6 language mode}}
  // expected-swift5-warning@-6 {{conformance of 'IsolatedSubscriptCollection' to protocol 'Collection' crosses into main actor-isolated code and can cause data races; this is an error in the Swift 6 language mode}}
  // expected-note@-7 3 {{turn data races into runtime errors with '@preconcurrency'}}
  nonisolated var startIndex: Int { 0 }
  nonisolated var endIndex: Int { 0 }
  subscript(position: Int) -> Int { 0 }
  // expected-note@-1 3 {{main actor-isolated subscript 'subscript(_:)' cannot satisfy nonisolated requirement}}
  // expected-note@-2 3 {{mark subscript 'subscript(_:)' 'nonisolated'}}

  typealias Iterator = IndexingIterator<IsolatedSubscriptCollection>
  typealias SubSequence = Slice<IsolatedSubscriptCollection>
}

// The class's isolation is inferred from the protocol, so nothing is diagnosed
// in Swift 5.
@MainActor
protocol MainActorSendableP: CustomStringConvertible, Sendable {
  func g() async
}

final class CInferredIsolation: MainActorSendableP {
  // expected-swift6-error@-1 {{conformance of 'CInferredIsolation' to protocol 'CustomStringConvertible' crosses into main actor-isolated code and can cause data races}}
  // expected-swift6-note@-2 {{turn data races into runtime errors with '@preconcurrency'}}
  func g() async {}
  var description: String { "" }
  // expected-swift6-note@-1 {{main actor-isolated property 'description' cannot satisfy nonisolated requirement}}
  // expected-swift6-note@-2 {{mark property 'description' 'nonisolated'}}
}

// FIXME: Restating the conformance in an extension makes it explicit rather
// than implied, so it's still inferred to be isolated.
final class CRestatedConformance: MainActorSendableP {
  // expected-error@-1 {{type 'CRestatedConformance' does not conform to protocol 'MainActorSendableP'}}
  // expected-error@-2 {{main actor-isolated conformance of 'CRestatedConformance' to 'CustomStringConvertible' cannot satisfy conformance requirement for a 'Sendable' type parameter 'Self'}}
  // expected-note@-3 {{requirement specified as 'Self' : 'CustomStringConvertible' [with Self = CRestatedConformance]}}
  func g() async {}
}

extension CRestatedConformance: CustomStringConvertible {
  var description: String { "" }
}

// MARK: - No 'Sendable' refinement: still inferred isolated

protocol RefinesPSync: PSync {}

@MainActor
final class CNoSendableRefinement: RefinesPSync {
  func f() {}
}

// 'Sendable' on the conforming type, rather than on a protocol that implies the
// conformance, doesn't stop the inference.
@MainActor
final class CSendableType: Sendable, RefinesPSync {
  func f() {}
}

// MARK: - Uses from a nonisolated context

nonisolated func usePIsolatedRequirement<T: PIsolatedRequirement>(_: T.Type) {}
nonisolated func usePAsync<T: PAsync>(_: T.Type) {}
nonisolated func useLeaf<T: Leaf>(_: T.Type) {}
nonisolated func useRandomAccessCollection<T: RandomAccessCollection>(_: T.Type) {}
nonisolated func usePSync<T: PSync>(_: T.Type) {}
nonisolated func useRefinesPSync<T: RefinesPSync>(_: T.Type) {}

nonisolated func testUsesFromNonisolatedContext() {
  usePIsolatedRequirement(CIsolatedRequirement.self)
  usePAsync(CAsyncWitness.self)
  useLeaf(CTwoRefinementsUp.self)
  useRandomAccessCollection(IsolatedSubscriptCollection.self)

  usePSync(CNoSendableRefinement.self)
  // expected-swift6-error@-1 {{main actor-isolated conformance of 'CNoSendableRefinement' to 'PSync' cannot be used in nonisolated context}}
  // expected-swift5-warning@-2 {{main actor-isolated conformance of 'CNoSendableRefinement' to 'PSync' cannot be used in nonisolated context; this is an error in the Swift 6 language mode}}
  useRefinesPSync(CNoSendableRefinement.self)
  // expected-swift6-error@-1 {{main actor-isolated conformance of 'CNoSendableRefinement' to 'RefinesPSync' cannot be used in nonisolated context}}
  // expected-swift5-warning@-2 {{main actor-isolated conformance of 'CNoSendableRefinement' to 'RefinesPSync' cannot be used in nonisolated context; this is an error in the Swift 6 language mode}}
  usePSync(CSendableType.self)
  // expected-swift6-error@-1 {{main actor-isolated conformance of 'CSendableType' to 'PSync' cannot be used in nonisolated context}}
  // expected-swift5-warning@-2 {{main actor-isolated conformance of 'CSendableType' to 'PSync' cannot be used in nonisolated context; this is an error in the Swift 6 language mode}}
}

//--- default_main_actor.swift

// Protocols are main actor-isolated by default in this mode; the ones below are
// explicitly 'nonisolated'.
nonisolated protocol P {
  func f()
}

nonisolated protocol SendableP: P, Sendable {}

nonisolated protocol RefinesP: P {}

// Conformance in an extension of a main actor-isolated type.
struct SExtension {}

extension SExtension: SendableP {
  // expected-swift6-error@-1 {{conformance of 'SExtension' to protocol 'P' crosses into main actor-isolated code and can cause data races}}
  // expected-swift5-warning@-2 {{conformance of 'SExtension' to protocol 'P' crosses into main actor-isolated code and can cause data races; this is an error in the Swift 6 language mode}}
  // expected-note@-3 {{turn data races into runtime errors with '@preconcurrency'}}
  @MainActor func f() {}
  // expected-note@-1 {{main actor-isolated instance method 'f()' cannot satisfy nonisolated requirement}}
}

// Conformance in an extension of a 'nonisolated' type.
nonisolated struct SNonisolated {}

extension SNonisolated: SendableP {
  // expected-swift6-error@-1 {{conformance of 'SNonisolated' to protocol 'P' crosses into main actor-isolated code and can cause data races}}
  // expected-swift5-warning@-2 {{conformance of 'SNonisolated' to protocol 'P' crosses into main actor-isolated code and can cause data races; this is an error in the Swift 6 language mode}}
  // expected-note@-3 {{turn data races into runtime errors with '@preconcurrency'}}
  @MainActor func f() {}
  // expected-note@-1 {{main actor-isolated instance method 'f()' cannot satisfy nonisolated requirement}}
}

// No 'Sendable' refinement: still inferred isolated.
struct SNoSendableRefinement {}

extension SNoSendableRefinement: RefinesP {
  @MainActor func f() {}
}

nonisolated func useP<T: P>(_: T.Type) {}

nonisolated func testUsesFromNonisolatedContext() {
  useP(SExtension.self)
  useP(SNonisolated.self)

  useP(SNoSendableRefinement.self)
  // expected-swift6-error@-1 {{main actor-isolated conformance of 'SNoSendableRefinement' to 'P' cannot be used in nonisolated context}}
  // expected-swift5-warning@-2 {{main actor-isolated conformance of 'SNoSendableRefinement' to 'P' cannot be used in nonisolated context; this is an error in the Swift 6 language mode}}
}
