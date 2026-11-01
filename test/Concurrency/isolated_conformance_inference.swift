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
