// RUN: %target-swift-frontend -typecheck -verify -target %target-swift-5.1-abi-triple -swift-version 6 -enable-upcoming-feature InferIsolatedConformances %s

// REQUIRES: concurrency
// REQUIRES: swift_feature_InferIsolatedConformances

protocol P {
  func f()
}

@SomeGlobalActor
protocol Q {
  func g()
}

@globalActor
actor SomeGlobalActor {
  static let shared = SomeGlobalActor()
}

@SomeGlobalActor
protocol R {
  func h()
}

@SomeGlobalActor
class CExplicit: P {
  func f() { } // okay! conformance above is isolated
}

// If the protocol itself is isolated, don't do anything.
extension CExplicit: Q {
  func g() { }
}

@SomeGlobalActor
class CViaNonisolatedWitness: P {
  nonisolated func f() { } // okay! conformance above is nonisolated via this witness
}

// expected-error@+3{{conformance of 'CNonIsolated' to protocol 'P' crosses into global actor 'SomeGlobalActor'-isolated code and can cause data races}}
// expected-note@+2{{turn data races into runtime errors with '@preconcurrency'}}
// expected-note@+1{{isolate this conformance to the global actor 'SomeGlobalActor' with '@SomeGlobalActor'}}{{33-33=@SomeGlobalActor }}
nonisolated class CNonIsolated: P {
  @SomeGlobalActor func f() { } // expected-note{{global actor 'SomeGlobalActor'-isolated instance method 'f()' cannot satisfy nonisolated requirement}}
}

// Conformances to GlobalActor are never inferred as isolated.
// expected-error@+4:8{{conformance of 'MainActorGlobalActor' to protocol 'GlobalActor' crosses into main actor-isolated code and can cause data races}}
// expected-note@+3:8{{turn data races into runtime errors with '@preconcurrency'}}{{none}}
@MainActor
@globalActor
struct MainActorGlobalActor {
  static let shared = SomeGlobalActor.shared // expected-note@:14{{main actor-isolated static property 'shared' cannot satisfy nonisolated requirement}}
  // expected-note@-1:14{{mark static property 'shared' 'nonisolated'}}{{3-3=nonisolated }}
}

@MainActor
@globalActor
struct MainActorGlobalActorNonisolatedShared {
  nonisolated static let shared = SomeGlobalActor.shared
}

func acceptSendablePMeta<T: Sendable & P>(_: T.Type) { }
func acceptSendableQMeta<T: Sendable & Q>(_: T.Type) { }

// @preconcurrency suppresses actor isolation inference
struct NotSendable: Equatable, Hashable {
}

@available(*, unavailable)
extension NotSendable: Sendable {}

extension NotSendable : Codable {}

@MainActor
struct TestDerivedCodable : @preconcurrency Codable {
  var x: NotSendable
}

nonisolated func testConformancesFromNonisolated(tdc: TestDerivedCodable) {
  let _: any P = CExplicit() // expected-error{{global actor 'SomeGlobalActor'-isolated conformance of 'CExplicit' to 'P' cannot be used in nonisolated context}}

  let _: any P = CNonIsolated()

  // Okay, these are nonisolated conformances.
  let _: any Q = CExplicit()

  let _: any P = CViaNonisolatedWitness()

  let _: any Codable = tdc
}

protocol P2 {
  func g()
}

struct DifferingConformances: @MainActor P {
  @MainActor func f() { }
}

extension DifferingConformances: @SomeGlobalActor P2 {
  @SomeGlobalActor func g() { }
}

@MainActor
class InferMeDefaults {
  var mainState: any P.Type = DifferingConformances.self
  var someGlobalActorState: any P2.Type = DifferingConformances.self // expected-error{{global actor 'SomeGlobalActor'-isolated default value in a main actor-isolated context}}
  var bothState: any (P & P2).Type = DifferingConformances.self // expected-error{{default argument cannot be both main actor-isolated and global actor 'SomeGlobalActor'-isolated}}
}

protocol PDerived: P {}

@MainActor struct ImpliedConformanceInference: PDerived { func f() {} }

// Used to cause a cycle because Iterator typealias could be found through makeIterator while trying
// determine `S: Sequence` isolation.

@MainActor
struct RecursiveCollection: RandomAccessCollection {
  nonisolated var startIndex: Int { 0 }
  nonisolated var endIndex: Int { 0 }
  nonisolated subscript(position: Int) -> Int { 0 }

  typealias Iterator = IndexingIterator<RecursiveCollection> // Ok
  typealias SubSequence = Slice<RecursiveCollection>
}

struct MyIndexingIterator<T: Collection & Sendable>: IteratorProtocol { // expected-note 2 {{requirement specified as 'T' : 'Collection' [with T = RecursiveCollectionIsolated]}}
  init(elements: T) { // expected-note {{'init(elements:)' declared here}}
  }

  mutating func next() -> T.Element? {
    nil
  }
}

@MainActor
struct RecursiveCollectionIsolated: RandomAccessCollection {
  nonisolated var startIndex: Int { 0 }
  nonisolated var endIndex: Int { 0 }
  nonisolated subscript(position: Int) -> Int { 0 }
  func makeIterator() -> MyIndexingIterator<RecursiveCollectionIsolated> {
    // expected-error@-1 {{main actor-isolated conformance of 'RecursiveCollectionIsolated' to 'Collection' cannot satisfy conformance requirement for a 'Sendable' type parameter 'T'}}
    MyIndexingIterator(elements: self)
    // expected-error@-1 2 {{main actor-isolated conformance of 'RecursiveCollectionIsolated' to 'Collection' cannot satisfy conformance requirement for a 'Sendable' type parameter}}
  }
  typealias Iterator = MyIndexingIterator<RecursiveCollectionIsolated>
  // expected-error@-1 {{main actor-isolated conformance of 'RecursiveCollectionIsolated' to 'Collection' cannot satisfy conformance requirement for a 'Sendable' type parameter 'T'}}
  typealias SubSequence = Slice<RecursiveCollectionIsolated>
}

// A witness doesn't make the conformance isolated if it isn't isolated, if it
// can be used from any isolation domain, or if its requirement is isolated the
// same way (so every use through the conformance is already in that domain).
// 'CViaNonisolatedWitness' above covers the first case.

// An immutable 'Sendable' stored property.
@MainActor
final class CViaSendableLet: Identifiable {
  let id = "id"
}

protocol HasValue {
  var value: Int { get }
}

// A 'Sendable' stored property of a struct.
@MainActor
struct SViaSendableStoredVar: HasValue {
  var value: Int
}

protocol MainActorRequirement {
  @MainActor func f()
}

@MainActor
struct SViaMatchingRequirement: MainActorRequirement {
  func f() { }
}

protocol SomeGlobalActorRequirement {
  @SomeGlobalActor func f()
}

// 'f()' is isolated to 'SomeGlobalActor' by its requirement, not to the main
// actor like the type.
@MainActor
struct SViaRequirementIsolation: SomeGlobalActorRequirement {
  func f() { }
}

// These witnesses still make the conformance isolated.

protocol HasStaticValue {
  static var value: Int { get }
}

// A 'static let' is lazily initialized global state.
@MainActor
final class CStaticLet: HasStaticValue {
  static let value = 1
}

class NonSendableClass { }

protocol HasNonSendableValue {
  var value: NonSendableClass { get }
}

@MainActor
final class CNonSendableLet: HasNonSendableValue {
  let value = NonSendableClass()
}

@MainActor
struct SComputedVar: HasValue {
  var value: Int { 1 }
}

protocol MainActorAndUnannotatedRequirements {
  @MainActor func f()
  func g()
}

// 'f()' matches its requirement, but 'g()' doesn't.
@MainActor
struct SMatchingAndIsolatedWitnesses: MainActorAndUnannotatedRequirements {
  func f() { }
  func g() { }
}

@MainActor
struct SDifferentActorWitness: SomeGlobalActorRequirement {
  @MainActor func f() { }
}

protocol AsyncRequirement {
  func f() async -> Int
}

@MainActor
final class CAsyncWitness: AsyncRequirement {
  func f() async -> Int { 0 }
}

nonisolated func testWitnessesFromNonisolated(
  sendableLet: CViaSendableLet, sendableStoredVar: SViaSendableStoredVar,
  matchingRequirement: SViaMatchingRequirement,
  requirementIsolation: SViaRequirementIsolation,
  staticLet: CStaticLet, nonSendableLet: CNonSendableLet,
  computedVar: SComputedVar, matchingAndIsolated: SMatchingAndIsolatedWitnesses,
  differentActor: SDifferentActorWitness, asyncWitness: CAsyncWitness
) {
  // Okay, these are nonisolated conformances.
  let _: any Identifiable = sendableLet
  let _: any HasValue = sendableStoredVar
  let _: any MainActorRequirement = matchingRequirement
  let _: any SomeGlobalActorRequirement = requirementIsolation

  let _: any HasStaticValue = staticLet // expected-error{{main actor-isolated conformance of 'CStaticLet' to 'HasStaticValue' cannot be used in nonisolated context}}
  let _: any HasNonSendableValue = nonSendableLet // expected-error{{main actor-isolated conformance of 'CNonSendableLet' to 'HasNonSendableValue' cannot be used in nonisolated context}}
  let _: any HasValue = computedVar // expected-error{{main actor-isolated conformance of 'SComputedVar' to 'HasValue' cannot be used in nonisolated context}}
  let _: any MainActorAndUnannotatedRequirements = matchingAndIsolated // expected-error{{main actor-isolated conformance of 'SMatchingAndIsolatedWitnesses' to 'MainActorAndUnannotatedRequirements' cannot be used in nonisolated context}}
  let _: any SomeGlobalActorRequirement = differentActor // expected-error{{main actor-isolated conformance of 'SDifferentActorWitness' to 'SomeGlobalActorRequirement' cannot be used in nonisolated context}}
  let _: any AsyncRequirement = asyncWitness // expected-error{{main actor-isolated conformance of 'CAsyncWitness' to 'AsyncRequirement' cannot be used in nonisolated context}}
}

// A conformance implied by a conformance to a protocol that inherits
// 'SendableMetatype' is not inferred to be isolated.

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
final class CAsyncWitnessSendableRefinement: SendablePAsync {
  func f() async {}
}

// A synchronous witness for a nonisolated requirement is diagnosed.
protocol SendableP: P, Sendable {}

@SomeGlobalActor
final class CSyncWitness: SendableP {
  // expected-error@-1 {{conformance of 'CSyncWitness' to protocol 'P' crosses into global actor 'SomeGlobalActor'-isolated code and can cause data races}}
  // expected-note@-2 {{turn data races into runtime errors with '@preconcurrency'}}
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
  // expected-error@-1 {{conformance of 'CTwoRefinementsUp' to protocol 'Leaf' crosses into main actor-isolated code and can cause data races}}
  // expected-note@-2 {{turn data races into runtime errors with '@preconcurrency'}}
  func leaf() {}
  // expected-note@-1 {{main actor-isolated instance method 'leaf()' cannot satisfy nonisolated requirement}}
  // expected-note@-2 {{mark instance method 'leaf()' 'nonisolated'}}
}

// Several implied conformances, with associated types that refer back to the
// conforming type.
protocol SendableRandomAccessCollection: RandomAccessCollection, Sendable {}

@MainActor
struct IsolatedSubscriptCollection: SendableRandomAccessCollection {
  // expected-error@-1 {{conformance of 'IsolatedSubscriptCollection' to protocol 'RandomAccessCollection' crosses into main actor-isolated code and can cause data races}}
  // expected-error@-2 {{conformance of 'IsolatedSubscriptCollection' to protocol 'BidirectionalCollection' crosses into main actor-isolated code and can cause data races}}
  // expected-error@-3 {{conformance of 'IsolatedSubscriptCollection' to protocol 'Collection' crosses into main actor-isolated code and can cause data races}}
  // expected-note@-4 3 {{turn data races into runtime errors with '@preconcurrency'}}
  nonisolated var startIndex: Int { 0 }
  nonisolated var endIndex: Int { 0 }
  subscript(position: Int) -> Int { 0 }
  // expected-note@-1 3 {{main actor-isolated subscript 'subscript(_:)' cannot satisfy nonisolated requirement}}
  // expected-note@-2 3 {{mark subscript 'subscript(_:)' 'nonisolated'}}

  typealias Iterator = IndexingIterator<IsolatedSubscriptCollection>
  typealias SubSequence = Slice<IsolatedSubscriptCollection>
}

// The class's isolation is inferred from the protocol.
@MainActor
protocol MainActorSendableP: CustomStringConvertible, Sendable {
  func g() async
}

final class CInferredIsolation: MainActorSendableP {
  // expected-error@-1 {{conformance of 'CInferredIsolation' to protocol 'CustomStringConvertible' crosses into main actor-isolated code and can cause data races}}
  // expected-note@-2 {{turn data races into runtime errors with '@preconcurrency'}}
  func g() async {}
  var description: String { "" }
  // expected-note@-1 {{main actor-isolated property 'description' cannot satisfy nonisolated requirement}}
  // expected-note@-2 {{mark property 'description' 'nonisolated'}}
}

// Restating the conformance in an extension doesn't make it inferred isolated,
// so the isolated witness is diagnosed.
final class CRestatedConformance: MainActorSendableP {
  func g() async {}
}

// FIXME: The suggestion to isolate the conformance doesn't help: an isolated
// conformance can't satisfy 'MainActorSendableP', which refines
// 'CustomStringConvertible' and 'Sendable'.
extension CRestatedConformance: CustomStringConvertible {
  // expected-error@-1:33 {{conformance of 'CRestatedConformance' to protocol 'CustomStringConvertible' crosses into main actor-isolated code and can cause data races}}
  // expected-note@-2:33 {{isolate this conformance to the main actor with '@MainActor'}}
  // expected-note@-3:33 {{turn data races into runtime errors with '@preconcurrency'}}
  var description: String { "" }
  // expected-note@-1:7 {{main actor-isolated property 'description' cannot satisfy nonisolated requirement}}
  // expected-note@-2:7 {{mark property 'description' 'nonisolated'}}
}

// The outcome doesn't depend on which of several conformances that imply the
// conformance comes first.
protocol RefinesPAsync: PAsync {}

@MainActor
final class CSendableRefinementFirst: SendablePAsync, RefinesPAsync {
  func f() async {}
}

@MainActor
final class CSendableRefinementLast: RefinesPAsync, SendablePAsync {
  func f() async {}
}

// ... or on whether the 'Sendable' refinement is stated in an extension.
@MainActor
final class CSendableRefinementInExtension: RefinesPAsync {
  func f() async {}
}

extension CSendableRefinementInExtension: SendablePAsync {}

// 'Sendable' on the conforming type, rather than on a protocol that implies the
// conformance, doesn't stop the inference.
@MainActor
final class CSendableType: Sendable, PDerived {
  func f() {}
}

nonisolated func usePIsolatedRequirement<T: PIsolatedRequirement>(_: T.Type) {}
nonisolated func usePAsync<T: PAsync>(_: T.Type) {}
nonisolated func useLeaf<T: Leaf>(_: T.Type) {}
nonisolated func useRandomAccessCollection<T: RandomAccessCollection>(_: T.Type) {}
nonisolated func useP<T: P>(_: T.Type) {}
nonisolated func usePDerived<T: PDerived>(_: T.Type) {}

nonisolated func testImpliedConformancesFromNonisolated() {
  usePIsolatedRequirement(CIsolatedRequirement.self)
  usePAsync(CAsyncWitnessSendableRefinement.self)
  useLeaf(CTwoRefinementsUp.self)
  useRandomAccessCollection(IsolatedSubscriptCollection.self)
  usePAsync(CSendableRefinementFirst.self)
  usePAsync(CSendableRefinementLast.self)
  usePAsync(CSendableRefinementInExtension.self)

  // No 'Sendable' refinement: still inferred isolated.
  useP(ImpliedConformanceInference.self)
  // expected-error@-1 {{main actor-isolated conformance of 'ImpliedConformanceInference' to 'P' cannot be used in nonisolated context}}
  usePDerived(ImpliedConformanceInference.self)
  // expected-error@-1 {{main actor-isolated conformance of 'ImpliedConformanceInference' to 'PDerived' cannot be used in nonisolated context}}
  useP(CSendableType.self)
  // expected-error@-1 {{main actor-isolated conformance of 'CSendableType' to 'P' cannot be used in nonisolated context}}
}
