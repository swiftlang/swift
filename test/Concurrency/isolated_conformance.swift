// RUN: %target-swift-frontend -typecheck -verify -target %target-swift-5.1-abi-triple -swift-version 5 -strict-concurrency=complete %s

// REQUIRES: concurrency

protocol P {
  func f()
}

// ----------------------------------------------------------------------------
// Definition of isolated conformances
// ----------------------------------------------------------------------------

// expected-warning@+3{{conformance of 'CWithNonIsolated' to protocol 'P' crosses into main actor-isolated code and can cause data races}}
// expected-note@+2{{isolate this conformance to the main actor with '@MainActor'}}{{25-25=@MainActor }}
@MainActor
class CWithNonIsolated: P {
  // expected-note@-1{{turn data races into runtime errors with '@preconcurrency'}}{{25-25=@preconcurrency }}
  func f() { } // expected-note{{main actor-isolated instance method 'f()' cannot satisfy nonisolated requirement}}
  // expected-note@-1{{mark instance method 'f()' 'nonisolated'}}{{3-3=nonisolated }}
}

// Make sure we correctly apply the conformance attribute for more complex
// inheritance entries.
do {
  protocol EmptyP {}
  struct Nested {
    protocol P { func f() }
    protocol Q { func f() }
  }
  do {
    // expected-warning@+3 {{conformance of 'S' to protocol 'P' crosses into main actor-isolated code and can cause data races}}
    // expected-note@+2 {{isolate this conformance to the main actor with '@MainActor'}}{{26-26=@MainActor }}
    // expected-note@+1 {{turn data races into runtime errors with '@preconcurrency'}}{{26-26=@preconcurrency }}
    @MainActor struct S: Nested.P {
      func f() {} // expected-note {{main actor-isolated instance method 'f()' cannot satisfy nonisolated requirement}}
      // expected-note@-1 {{mark instance method 'f()' 'nonisolated'}}{{7-7=nonisolated }}
    }
  }
  do {
    // Attribute inserted *before* '@unsafe'.
    @MainActor struct S: @unsafe Nested.P {
    // expected-warning@-1 {{conformance of 'S' to protocol 'P' crosses into main actor-isolated code and can cause data races}}
    // expected-note@-2 {{isolate this conformance to the main actor with '@MainActor'}}{{26-26=@MainActor }}
    // expected-note@-3 {{turn data races into runtime errors with '@preconcurrency'}}{{26-26=@preconcurrency }}
      func f() {} // expected-note {{main actor-isolated instance method 'f()' cannot satisfy nonisolated requirement}}
      // expected-note@-1 {{mark instance method 'f()' 'nonisolated'}}{{7-7=nonisolated }}
    }
  }
  do {
    // expected-warning@+4 {{conformance of 'S' to protocol 'Q' crosses into main actor-isolated code and can cause data races}}
    // expected-warning@+3 {{conformance of 'S' to protocol 'P' crosses into main actor-isolated code and can cause data races}}
    // expected-note@+2 2 {{isolate this conformance to the main actor with '@MainActor'}}{{26-26=@MainActor }}
    // expected-note@+1 2 {{turn data races into runtime errors with '@preconcurrency'}}{{26-26=@preconcurrency }}
    @MainActor struct S: Nested.P & Nested.Q {
      func f() {} // expected-note 2 {{main actor-isolated instance method 'f()' cannot satisfy nonisolated requirement}}
      // expected-note@-1 2 {{mark instance method 'f()' 'nonisolated'}}{{7-7=nonisolated }}
    }
  }
  do {
    // FIXME: We shouldn't be applying nonisolated to both protocols.
    // expected-warning@+3 {{conformance of 'S' to protocol 'P' crosses into main actor-isolated code and can cause data races}}
    // expected-note@+2 {{isolate this conformance to the main actor with '@MainActor'}}{{26-26=@MainActor }}
    // expected-note@+1 {{turn data races into runtime errors with '@preconcurrency'}}{{26-26=@preconcurrency }}
    @MainActor struct S: Nested.P & EmptyP  {
      func f() {} // expected-note {{main actor-isolated instance method 'f()' cannot satisfy nonisolated requirement}}
      // expected-note@-1 {{mark instance method 'f()' 'nonisolated'}}{{7-7=nonisolated }}
    }
  }
}

actor SomeActor { }

// Isolated conformances need a global-actor-constrained type.
class CNonIsolated: @MainActor P {
  func f() { }
}

extension SomeActor: @MainActor P {
  @MainActor func f() { }
}

@globalActor
struct SomeGlobalActor {
  static let shared = SomeActor()
}

// Isolation of the conformance can be different from that of the enclosing
// type, so long as the witnesses match up.
@MainActor
class CMismatchedIsolation: @SomeGlobalActor P {
  @SomeGlobalActor func f() { }
}

@MainActor
class C: @MainActor P {
  func f() { } // okay
}

// Associated conformances with isolation

protocol Q {
  associatedtype A: P
}

// expected-warning@+2{{conformance of 'SMissingIsolation' to protocol 'Q' crosses into main actor-isolated code and can cause data races}}
@MainActor
struct SMissingIsolation: Q {
  // expected-note@-1{{conformance depends on main actor-isolated conformance of 'C' to protocol 'P'}}
  // expected-note@-2{{isolate this conformance to the main actor with '@MainActor'}}
  typealias A = C
}

struct PWrapper<T: P>: P {
  func f() { }
}

// expected-warning@+2{{conformance of 'SMissingIsolationViaWrapper' to protocol 'Q' crosses into main actor-isolated code and can cause data races}}
@MainActor
struct SMissingIsolationViaWrapper: Q {
  // expected-note@-1{{conformance depends on main actor-isolated conformance of 'C' to protocol 'P'}}
  // expected-note@-2{{isolate this conformance to the main actor with '@MainActor'}}{{37-37=@MainActor }}
  typealias A = PWrapper<C>
}

@SomeGlobalActor
class C2: @SomeGlobalActor P {
  func f() { }
}

@MainActor
struct S: @MainActor Q {
  typealias A = C
}

// expected-warning@+3{{conformance of 'SMismatchedActors' to protocol 'Q' crosses into global actor 'SomeGlobalActor'-isolated code and can cause data races}}
// expected-note@+2{{conformance depends on global actor 'SomeGlobalActor'-isolated conformance of 'C2' to protocol 'P'}}
@MainActor
struct SMismatchedActors: @MainActor Q {
  typealias A = C2
}

protocol PSendable: P, Sendable { }

// expected-error@+2{{type 'PSendableS' does not conform to protocol 'PSendable'}}
// expected-error@+1{{main actor-isolated conformance of 'PSendableS' to 'P' cannot satisfy conformance requirement for a 'Sendable' type parameter 'Self'}}
struct PSendableS: @MainActor PSendable { // expected-note{{requirement specified as 'Self' : 'P' [with Self = PSendableS]}}
  func f() { }
}

protocol R: SendableMetatype {
  func f()
}

// expected-error@+1{{cannot form main actor-isolated conformance of 'RSendableSMainActor' to SendableMetatype-inheriting protocol 'R'}}
@MainActor struct RSendableSMainActor: @MainActor R {
  func f() { }
}

// ----------------------------------------------------------------------------
// Use checking of isolated conformances.
// ----------------------------------------------------------------------------

// expected-note@+1{{requirement specified as 'T' : 'P' [with T = C]}}
struct PSendableWrapper<T: P & Sendable>: P {
  func f() { }
}

// expected-note@+1{{requirement specified as 'T' : 'P' [with T = C]}}
struct PSendableMetaWrapper<T: P & SendableMetatype>: P {
  func f() { }
}

@MainActor
func testIsolationConformancesInTypes() {
  typealias A1 = PWrapper<C>
  typealias A2 = PSendableWrapper<C> // expected-error{{isolated conformance of 'C' to 'P' cannot satisfy conformance requirement for a 'Sendable' type parameter 'T'}}
  typealias A3 = PSendableMetaWrapper<C> // expected-error{{isolated conformance of 'C' to 'P' cannot satisfy conformance requirement for a 'SendableMetatype' type parameter 'T'}}
}

func acceptP<T: P>(_: T) { }

func acceptSendableP<T: Sendable & P>(_: T) { }
// expected-note@-1{{'acceptSendableP' declared here}}

func acceptSendableMetaP<T: SendableMetatype & P>(_: T) { }
// expected-note@-1 3{{'acceptSendableMetaP' declared here}}

@MainActor
func testIsolationConformancesInCall(c: C) {
  acceptP(c) // okay

  acceptSendableP(c) // expected-error{{main actor-isolated conformance of 'C' to 'P' cannot satisfy conformance requirement for a 'Sendable' type parameter}}
  acceptSendableMetaP(c) // expected-error{{isolated conformance of 'C' to 'P' cannot satisfy conformance requirement for a 'Sendable' type parameter}}
}

@MainActor
func testIsolatedConformancesOfActor(a: SomeActor) {
  acceptP(a)
  acceptSendableMetaP(a) // expected-error{{main actor-isolated conformance of 'SomeActor' to 'P' cannot satisfy conformance requirement for a 'Sendable' type parameter}}
}

@SomeGlobalActor
func testIsolatedConformancesOfOtherGlobalActor(c: CMismatchedIsolation) {
  acceptP(c)
  acceptSendableMetaP(c)  // expected-error{{global actor 'SomeGlobalActor'-isolated conformance of 'CMismatchedIsolation' to 'P' cannot satisfy conformance requirement for a 'Sendable' type parameter}}
}

func testIsolationConformancesFromOutside(c: C) {
  acceptP(c) // expected-warning{{main actor-isolated conformance of 'C' to 'P' cannot be used in nonisolated context}}
  let _: any P = c // expected-warning{{main actor-isolated conformance of 'C' to 'P' cannot be used in nonisolated context}}
  let _ = PWrapper<C>() // expected-warning{{main actor-isolated conformance of 'C' to 'P' cannot be used in nonisolated context}}
}

protocol HasAssociatedType {
  associatedtype A
}

func acceptHasAssocWithP<T: HasAssociatedType>(_: T) where T.A: P { }

func acceptSendableHasAssocWithP<T: Sendable & HasAssociatedType>(_: T) where T.A: P { }
// expected-note@-1{{'acceptSendableHasAssocWithP' declared here}}


struct HoldsC: HasAssociatedType {
  typealias A = C
}

extension HasAssociatedType {
  static func acceptAliased<T: P>(_: T.Type) where A == T { }
}

extension HasAssociatedType where Self: Sendable {
  static func acceptSendableAliased<T: P>(_: T.Type) where A == T { }
}

func testIsolatedConformancesOnAssociatedTypes(hc: HoldsC, c: C) {
  acceptHasAssocWithP(hc)
  acceptSendableHasAssocWithP(hc) // expected-error{{main actor-isolated conformance of 'C' to 'P' cannot satisfy conformance requirement for a 'Sendable' type parameter }}

  HoldsC.acceptAliased(C.self) // okay

  // FIXME: the following should produce an error, because the isolated
  // conformance of C: P can cross isolation boundaries via the Sendable Self's
  // associated type.
  HoldsC.acceptSendableAliased(C.self)
}


struct MyHashable: @MainActor Hashable {
  var counter = 0
}

@concurrent func testMyHashableSet() async {
  let _: Set<MyHashable> = [] // expected-warning{{main actor-isolated conformance of 'MyHashable' to 'Hashable' cannot be used in @concurrent context}}
}

// Like Identifiable!
protocol Distinguishable {
  associatedtype ID: Hashable
  var id: Self.ID { get }
}

// expected-warning@+4{{conformance of 'MyDistinguishable' to protocol 'Distinguishable' crosses into main actor-isolated code and can cause data races}}
// expected-note@+3{{isolate this conformance to the main actor with '@MainActor'}}{{26-26=@MainActor }}
// expected-note@+2{{turn data races into runtime errors with '@preconcurrency'}}{{26-26=@preconcurrency }}
@MainActor
class MyDistinguishable: Distinguishable {
  var id = "" // expected-note{{main actor-isolated property 'id' cannot satisfy nonisolated requirement}}
  // expected-note@-1{{change property 'id' to a 'nonisolated let' constant}}{{3-6=nonisolated let}}
  var name = "my distinguishable"
}

protocol HasMutableVar {
  associatedtype Val
  var val: Self.Val { get set }
}

// expected-warning@+7{{conformance of 'ImmutableWithSeparateMutable' to protocol 'Distinguishable' crosses into main actor-isolated code and can cause data races}}
// expected-note@+6{{isolate this conformance to the main actor with '@MainActor'}}
// expected-note@+5{{turn data races into runtime errors with '@preconcurrency'}}
// expected-warning@+4{{conformance of 'ImmutableWithSeparateMutable' to protocol 'HasMutableVar' crosses into main actor-isolated code and can cause data races}}
// expected-note@+3{{isolate this conformance to the main actor with '@MainActor'}}
// expected-note@+2{{turn data races into runtime errors with '@preconcurrency'}}
@MainActor
class ImmutableWithSeparateMutable: Distinguishable, HasMutableVar {
  var id = "" // expected-note{{main actor-isolated property 'id' cannot satisfy nonisolated requirement}}
  // expected-note@-1{{change property 'id' to a 'nonisolated let' constant}}{{3-6=nonisolated let}}
  var val = 42 // expected-note{{main actor-isolated property 'val' cannot satisfy nonisolated requirement}}
}

protocol MixedMutability {
  var name: String { get }
  var count: Int { get set }
}

// expected-warning@+4{{conformance of 'MixedMutabilityWitness' to protocol 'MixedMutability' crosses into main actor-isolated code and can cause data races}}
// expected-note@+3{{isolate this conformance to the main actor with '@MainActor'}}
// expected-note@+2{{turn data races into runtime errors with '@preconcurrency'}}
@MainActor
class MixedMutabilityWitness: MixedMutability {
  var name = "hello" // expected-note{{main actor-isolated property 'name' cannot satisfy nonisolated requirement}}
  // expected-note@-1{{change property 'name' to a 'nonisolated let' constant}}{{3-6=nonisolated let}}
  var count = 0 // expected-note{{main actor-isolated property 'count' cannot satisfy nonisolated requirement}}
}

protocol MultipleReadOnly {
  var first: String { get }
  var second: Int { get }
}

// expected-warning@+4{{conformance of 'MultipleReadOnlyWitness' to protocol 'MultipleReadOnly' crosses into main actor-isolated code and can cause data races}}
// expected-note@+3{{isolate this conformance to the main actor with '@MainActor'}}
// expected-note@+2{{turn data races into runtime errors with '@preconcurrency'}}
@MainActor
class MultipleReadOnlyWitness: MultipleReadOnly {
  var first = "a" // expected-note{{main actor-isolated property 'first' cannot satisfy nonisolated requirement}}
  // expected-note@-1{{change property 'first' to a 'nonisolated let' constant}}{{3-6=nonisolated let}}
  var second = 1 // expected-note{{main actor-isolated property 'second' cannot satisfy nonisolated requirement}}
  // expected-note@-1{{change property 'second' to a 'nonisolated let' constant}}{{3-6=nonisolated let}}
  var third = true
}

// expected-warning@+4{{conformance of 'ComputedWitness' to protocol 'Distinguishable' crosses into main actor-isolated code and can cause data races}}
// expected-note@+3{{isolate this conformance to the main actor with '@MainActor'}}
// expected-note@+2{{turn data races into runtime errors with '@preconcurrency'}}
@MainActor
class ComputedWitness: Distinguishable {
  var id: String { "computed" } // expected-note{{main actor-isolated property 'id' cannot satisfy nonisolated requirement}}
  // expected-note@-1{{mark property 'id' 'nonisolated'}}{{3-3=nonisolated }}
}

@propertyWrapper
struct Wrapped<Value> {
  var wrappedValue: Value
}

// expected-warning@+4{{conformance of 'WrappedWitness' to protocol 'Distinguishable' crosses into main actor-isolated code and can cause data races}}
// expected-note@+3{{isolate this conformance to the main actor with '@MainActor'}}
// expected-note@+2{{turn data races into runtime errors with '@preconcurrency'}}
@MainActor
class WrappedWitness: Distinguishable {
  @Wrapped var id: String = "wrapped" // expected-note{{main actor-isolated property 'id' cannot satisfy nonisolated requirement}}
}

// expected-warning@+4{{conformance of 'ComputedGetSetWitness' to protocol 'HasMutableVar' crosses into main actor-isolated code and can cause data races}}
// expected-note@+3{{isolate this conformance to the main actor with '@MainActor'}}
// expected-note@+2{{turn data races into runtime errors with '@preconcurrency'}}
@MainActor
class ComputedGetSetWitness: HasMutableVar {
  var val: Int { // expected-note{{main actor-isolated property 'val' cannot satisfy nonisolated requirement}}
  // expected-note@-1{{mark property 'val' 'nonisolated'}}{{3-3=nonisolated }}
    get { 42 }
    set { }
  }
}

// Check cases where generic parameter prohibits use of isolated conformances by conforming to Sendable/SendableMetatype

// expected-note@+1 32 {{requirement specified as 'T' : 'P' [with T = C]}}
struct SendableWrapper<T: P & Sendable> { }

typealias GlobalTypeAliasTest = SendableWrapper<C> // expected-error{{main actor-isolated conformance of 'C' to 'P' cannot satisfy conformance requirement for a 'Sendable' type parameter 'T'}}

struct GlobalVarTest {
  var prop: SendableWrapper<C>? = nil // expected-error{{main actor-isolated conformance of 'C' to 'P' cannot satisfy conformance requirement for a 'Sendable' type parameter 'T'}}
  init() { }
}

struct GlobalFuncParamTest {
  func method(_: SendableWrapper<C>) { } // expected-error{{main actor-isolated conformance of 'C' to 'P' cannot satisfy conformance requirement for a 'Sendable' type parameter 'T'}}
}

struct GlobalFuncResultTest {
  func method() -> SendableWrapper<C> { fatalError() } // expected-error{{main actor-isolated conformance of 'C' to 'P' cannot satisfy conformance requirement for a 'Sendable' type parameter 'T'}}
}

struct GlobalSubscriptTest {
  subscript(_: SendableWrapper<C>) -> Int { 0 } // expected-error{{main actor-isolated conformance of 'C' to 'P' cannot satisfy conformance requirement for a 'Sendable' type parameter 'T'}}
}

struct GlobalInitTest {
  init(_: SendableWrapper<C>) { } // expected-error{{main actor-isolated conformance of 'C' to 'P' cannot satisfy conformance requirement for a 'Sendable' type parameter 'T'}}
}

enum GlobalEnumCaseTest {
  case payload(SendableWrapper<C>) // expected-error{{main actor-isolated conformance of 'C' to 'P' cannot satisfy conformance requirement for a 'Sendable' type parameter 'T'}}
}

struct SubscriptReturnTest {
  subscript(i: Int) -> SendableWrapper<C> { fatalError() }
  // expected-error@-1 {{main actor-isolated conformance of 'C' to 'P' cannot satisfy conformance requirement for a 'Sendable' type parameter 'T'}}
}

protocol BaseProto {}
protocol ProtoWithInherited: BaseProto where Self.Assoc == SendableWrapper<C> {
  // expected-error@-1 {{main actor-isolated conformance of 'C' to 'P' cannot satisfy conformance requirement for a 'Sendable' type parameter 'T'}}
  associatedtype Assoc
}

protocol HasConstrainedAssoc {
  associatedtype A: Sequence where A.Element == SendableWrapper<C>
  // expected-error@-1 {{main actor-isolated conformance of 'C' to 'P' cannot satisfy conformance requirement for a 'Sendable' type parameter 'T'}}
}

protocol HasDefaultAssoc {
  associatedtype B = SendableWrapper<C>
  // expected-error@-1 {{main actor-isolated conformance of 'C' to 'P' cannot satisfy conformance requirement for a 'Sendable' type parameter 'T'}}
}

struct NominalWhereTest<U> where U == SendableWrapper<C> {
  // expected-warning@-1 {{same-type requirement makes generic parameter 'U' non-generic; this is an error in the Swift 6 language mode}}
  // expected-error@-2 {{main actor-isolated conformance of 'C' to 'P' cannot satisfy conformance requirement for a 'Sendable' type parameter 'T'}}
}

@freestanding(expression)
macro paramMacro(_ x: SendableWrapper<C>) = #externalMacro(module: "M", type: "T")
// expected-error@-1 {{main actor-isolated conformance of 'C' to 'P' cannot satisfy conformance requirement for a 'Sendable' type parameter 'T'}}
// expected-warning@-2 {{external macro implementation type 'M.T' could not be found for macro 'paramMacro'; plugin for module 'M' not found}}

@freestanding(expression)
macro resultMacro() -> SendableWrapper<C> = #externalMacro(module: "M", type: "T")
// expected-error@-1 {{main actor-isolated conformance of 'C' to 'P' cannot satisfy conformance requirement for a 'Sendable' type parameter 'T'}}
// expected-warning@-2 {{external macro implementation type 'M.T' could not be found for macro 'resultMacro()'; plugin for module 'M' not found}}

actor GlobalActorBackingActor {
  static let shared = GlobalActorBackingActor()
}

@globalActor
struct GenGlobalActor<T: P & Sendable> { // expected-note 2 {{requirement specified as 'T' : 'P' [with T = C]}}
  static var shared: GlobalActorBackingActor { GlobalActorBackingActor.shared }
  // expected-warning@-1 {{GlobalActor witness static property 'shared' may return different actor instances, which would lead to global actor isolation violations}}
  // expected-note@-2 {{declare it as 'static let' to guarantee a stable instance}}
  // expected-note@-3 {{if this property always returns the same instance, silence the warning with '@diagnose(UnstableGlobalActorShared, as: ignored)'}}
}

@GenGlobalActor<C>
// expected-error@-1 {{main actor-isolated conformance of 'C' to 'P' cannot satisfy conformance requirement for a 'Sendable' type parameter 'T'}}
struct UsesGenGlobalActor {}

func testLocalDeclTests() {
  typealias LocalTypeAliasTest = SendableWrapper<C> // expected-error{{main actor-isolated conformance of 'C' to 'P' cannot satisfy conformance requirement for a 'Sendable' type parameter 'T'}}

  let localVar: SendableWrapper<C>? = nil // expected-error{{main actor-isolated conformance of 'C' to 'P' cannot satisfy conformance requirement for a 'Sendable' type parameter 'T'}}
  _ = localVar

  func local(_: SendableWrapper<C>) { } // expected-error{{main actor-isolated conformance of 'C' to 'P' cannot satisfy conformance requirement for a 'Sendable' type parameter 'T'}}

  struct LocalNestedTypeTest {
    var prop: SendableWrapper<C>? = nil // expected-error{{main actor-isolated conformance of 'C' to 'P' cannot satisfy conformance requirement for a 'Sendable' type parameter 'T'}}
    init() { }
  }

  var computed: SendableWrapper<C> {
    // expected-error@-1 {{main actor-isolated conformance of 'C' to 'P' cannot satisfy conformance requirement for a 'Sendable' type parameter 'T'}}
    get { fatalError() }
  }
  
  class A<T: P & Sendable> {}
  // expected-note@-1 {{requirement specified as 'T' : 'P' [with T = C]}}

  class B: A<C> {}
  // expected-error@-1 {{main actor-isolated conformance of 'C' to 'P' cannot satisfy conformance requirement for a 'Sendable' type parameter 'T'}}

  @resultBuilder
  struct Builder<T: P & Sendable> { // expected-note {{requirement specified as 'T' : 'P' [with T = C]}}
    static func buildBlock(_: T) {}
  }

  func usesBuilder(@Builder<C> _: () -> C) {}
  // expected-error@-1 {{main actor-isolated conformance of 'C' to 'P' cannot satisfy conformance requirement for a 'Sendable' type parameter 'T'}}

  @propertyWrapper
  struct WrapperValue<Value: P & Sendable> { // expected-note {{requirement specified as 'Value' : 'P' [with Value = C]}}
    var wrappedValue: Value

    init(wrappedValue: Value) { // expected-note {{'init(wrappedValue:)' declared here}}
      self.wrappedValue = wrappedValue
    }
  }

  struct WrapperTest {
    @WrapperValue<C> var value = .init()
    // expected-error@-1 2 {{main actor-isolated conformance of 'C' to 'P' cannot satisfy conformance requirement for a 'Sendable' type parameter}}
  }
}

struct TestExt<T> {
}

extension TestExt where T == SendableWrapper<C> {
  // expected-error@-1 {{main actor-isolated conformance of 'C' to 'P' cannot satisfy conformance requirement for a 'Sendable' type parameter 'T'}}
}

struct ClosureThrownTypeError<E: P & Sendable>: Error { }
// expected-note@-1 {{requirement specified as 'E' : 'P' [with E = C]}}

func testExpressionContext<T>(v: T) {
  _ = SendableWrapper<C>.self
  // expected-error@-1 {{main actor-isolated conformance of 'C' to 'P' cannot satisfy conformance requirement for a 'Sendable' type parameter 'T'}}

  _ = (v as? SendableWrapper<C>)
  // expected-error@-1 {{main actor-isolated conformance of 'C' to 'P' cannot satisfy conformance requirement for a 'Sendable' type parameter 'T'}}
  _ = v as! SendableWrapper<C>
  // expected-error@-1 {{main actor-isolated conformance of 'C' to 'P' cannot satisfy conformance requirement for a 'Sendable' type parameter 'T'}}

  func test<U>() -> SendableWrapper<U> { // expected-note 2 {{'test()' declared here}}
  }

  guard let _: SendableWrapper<C> = test() else {
    // expected-error@-1 {{main actor-isolated conformance of 'C' to 'P' cannot satisfy conformance requirement for a 'Sendable' type parameter}}
    return
  }

  let _ = {
    guard let _: SendableWrapper<C> = test() else {
      // expected-error@-1 {{main actor-isolated conformance of 'C' to 'P' cannot satisfy conformance requirement for a 'Sendable' type parameter}}
      return
    }
  }

  if case is SendableWrapper<C> = v {
    // expected-error@-1 {{main actor-isolated conformance of 'C' to 'P' cannot satisfy conformance requirement for a 'Sendable' type parameter 'T'}}
  }

  let _ = {
    switch v {
    case is SendableWrapper<C>:
      // expected-error@-1 {{main actor-isolated conformance of 'C' to 'P' cannot satisfy conformance requirement for a 'Sendable' type parameter 'T'}}
      break
    default:
      break
    }

    guard case is SendableWrapper<C> = v else {
      // expected-error@-1 {{main actor-isolated conformance of 'C' to 'P' cannot satisfy conformance requirement for a 'Sendable' type parameter 'T'}}
      return
    }
  }

  let _ = { (a: SendableWrapper<C>) in
    // expected-error@-1 {{main actor-isolated conformance of 'C' to 'P' cannot satisfy conformance requirement for a 'Sendable' type parameter}}
  }

  let _ = { () -> SendableWrapper<C> in
    // expected-error@-1 {{main actor-isolated conformance of 'C' to 'P' cannot satisfy conformance requirement for a 'Sendable' type parameter}}
  }

  let _ = { () throws(ClosureThrownTypeError<C>) -> Void in
    // expected-error@-1 {{main actor-isolated conformance of 'C' to 'P' cannot satisfy conformance requirement for a 'Sendable' type parameter}}
  }

  let _ = { @GenGlobalActor<C> () -> Void in
    // expected-error@-1 {{main actor-isolated conformance of 'C' to 'P' cannot satisfy conformance requirement for a 'Sendable' type parameter}}
  }

  func makeWrapper() -> SendableWrapper<C> { fatalError() }
  // expected-error@-1 {{main actor-isolated conformance of 'C' to 'P' cannot satisfy conformance requirement for a 'Sendable' type parameter 'T'}}
  let inferredGlobal = makeWrapper()
  // expected-error@-1 {{main actor-isolated conformance of 'C' to 'P' cannot satisfy conformance requirement for a 'Sendable' type parameter 'T'}}

  typealias X<U: P> = SendableWrapper<U>
  _ = X<C>.self
  // expected-error@-1 {{main actor-isolated conformance of 'C' to 'P' cannot satisfy conformance requirement for a 'Sendable' type parameter 'T'}}

  typealias Y<U: P & SendableMetatype> = SendableWrapper<U> // expected-note {{requirement specified as 'U' : 'P' [with U = C]}}
  _ = Y<C>.self
  // expected-error@-1 {{main actor-isolated conformance of 'C' to 'P' cannot satisfy conformance requirement for a 'SendableMetatype' type parameter 'U'}}
  // expected-error@-2 {{main actor-isolated conformance of 'C' to 'P' cannot satisfy conformance requirement for a 'Sendable' type parameter 'T'}}
}

struct SendableWrapperError<T: P & Sendable>: Error { }
// expected-note@-1 {{'init()' declared here}}
// expected-note@-2 {{requirement specified as 'T' : 'P' [with T = C]}}

func testCatchAsPattern() {
  do {
    throw SendableWrapperError<C>()
    // expected-error@-1 {{main actor-isolated conformance of 'C' to 'P' cannot satisfy conformance requirement for a 'Sendable' type parameter}}
  } catch let e as SendableWrapperError<C> {
    // expected-error@-1 {{main actor-isolated conformance of 'C' to 'P' cannot satisfy conformance requirement for a 'Sendable' type parameter 'T'}}
    _ = e
  } catch {
  }
}

enum GenericPatternEnum<T: P & Sendable> { // expected-note {{requirement specified as 'T' : 'P' [with T = C]}}
  case foo(T)
  case bar
}

func testEnumElementPatternParentType(_ x: Any) {
  switch x {
  case GenericPatternEnum<C>.bar:
    // expected-error@-1 {{main actor-isolated conformance of 'C' to 'P' cannot satisfy conformance requirement for a 'Sendable' type parameter 'T'}}
    break
  default:
    break
  }
}

// https://github.com/swiftlang/swift/issues/91755
// Erasure must look through a specialized conformance to its substitutions.

struct GenericP<T: P>: P {
  func f() { }
}

func testErasureOfSpecializedConformance(_ g: GenericP<C>) {
  _ = g as any P
  // expected-warning@-1{{main actor-isolated conformance of 'C' to 'P' cannot be used in nonisolated context}}
  _ = GenericP<C>.self as any P.Type
  // expected-warning@-1{{main actor-isolated conformance of 'C' to 'P' cannot be used in nonisolated context}}
}
