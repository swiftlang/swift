// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -swift-version 6 -parse-as-library %s -emit-sil -o /dev/null -verify -strict-concurrency=complete

// REQUIRES: concurrency

@MainActor
protocol GloballyIsolated {}

// expected-note@+1 {{class 'NonSendable' does not conform to the 'Sendable' protocol}}
class NonSendable {}

@propertyWrapper struct P {
  var wrappedValue = 0
}

// MARK: - Structs

struct ImplicitlySendable {
  var a: Int

  // always okay
  nonisolated let b = 0
  nonisolated var c: Int { 0 }
  nonisolated var d = 0

  // never okay
  nonisolated lazy var e = 0  // expected-error {{'nonisolated' is not supported on lazy properties}}
  @P nonisolated var f = 0  // expected-error {{'nonisolated' is not supported on properties with property wrappers}}
}

struct ImplicitlyNonSendable {
  let a: NonSendable

  // always okay
  nonisolated let b = 0
  nonisolated var c: Int { 0 }
  nonisolated var d = 0

  // never okay
  nonisolated lazy var e = 0  // expected-error {{'nonisolated' is not supported on lazy properties}}
  @P nonisolated var f = 0  // expected-error {{'nonisolated' is not supported on properties with property wrappers}}
}

public struct PublicSendable: Sendable {
  // always okay
  nonisolated let b = 0
  nonisolated var c: Int { 0 }
  nonisolated var d = 0

  // never okay
  nonisolated lazy var e = 0  // expected-error {{'nonisolated' is not supported on lazy properties}}
  @P nonisolated var f = 0  // expected-error {{'nonisolated' is not supported on properties with property wrappers}}
}

public struct PublicNonSendable {
  // always okay
  nonisolated let b = 0
  nonisolated var c: Int { 0 }
  nonisolated var d = 0

  // never okay
  nonisolated lazy var e = 0  // expected-error {{'nonisolated' is not supported on lazy properties}}
  @P nonisolated var f = 0  // expected-error {{'nonisolated' is not supported on properties with property wrappers}}
}


nonisolated struct StructRemovesGlobalActor: GloballyIsolated {
  var x: NonSendable
  var y: Int = 1

  init(x: NonSendable) {
    self.x = x // okay
  }

  struct Nested: GloballyIsolated {
    // expected-note@+1 {{mutation of this property is only permitted within the actor}}
    var z: NonSendable
    nonisolated init(z: NonSendable) {
      // expected-error@+1 {{main actor-isolated property 'z' can not be mutated from a nonisolated context}}
      self.z = z
    }
  }
}

@MainActor struct S {
  var value: NonSendable // globally-isolated
  struct Nested {} // 'Nested' is not @MainActor-isolated
}

// expected-note@+1 {{calls to global function 'requireMain()' from outside of its actor context are implicitly asynchronous}}
@MainActor func requireMain() {}

nonisolated struct S1: GloballyIsolated {
  var x: NonSendable
  func f() {
    // expected-error@+1 {{call to main actor-isolated global function 'requireMain()' in a synchronous nonisolated context}}
    requireMain()
  }
}

// MARK: - Protocols

nonisolated protocol Refined: GloballyIsolated {}
nonisolated protocol WhyNot {}

nonisolated protocol NonisolatedWithMembers {
  func test()
}

struct A: Refined {
  var x: NonSendable
  init(x: NonSendable) {
    self.x = x // okay
  }

  init() {
    self.x = NonSendable()
  }

  func f() {}
}

@MainActor protocol ExplicitGlobalActor: Refined {}

struct IsolatedA: ExplicitGlobalActor {
  // expected-note@+2 {{main actor isolation inferred from conformance to protocol 'ExplicitGlobalActor'}}
  // expected-note@+1 {{calls to instance method 'g()' from outside of its actor context are implicitly asynchronous}}
  func g() {}
}

struct IsolatedB: Refined, ExplicitGlobalActor {
  // expected-note@+2 {{calls to instance method 'h()' from outside of its actor context are implicitly asynchronous}}
  // expected-note@+1 {{main actor isolation inferred from conformance to protocol 'ExplicitGlobalActor'}}
  func h() {}
}

struct IsolatedC: WhyNot, GloballyIsolated {
  // expected-note@+2 {{calls to instance method 'k()' from outside of its actor context are implicitly asynchronous}}
  // expected-note@+1 {{main actor isolation inferred from conformance to protocol 'GloballyIsolated'}}
  func k() {}
}

struct IsolatedCFlipped: GloballyIsolated, WhyNot {
  // expected-note@+2 {{calls to instance method 'k2()' from outside of its actor context are implicitly asynchronous}}
  // expected-note@+1 {{main actor isolation inferred from conformance to protocol 'GloballyIsolated'}}
  func k2() {}
}

struct NonisolatedStruct {
  func callF() {
    return A().f() // okay, 'A' is non-isolated.
  }

  // expected-note@+1 {{add '@MainActor' to make instance method 'callG()' part of global actor 'MainActor'}}
  func callG() {
    // expected-error@+1{{call to main actor-isolated instance method 'g()' in a synchronous nonisolated context}}
    return IsolatedA().g()
  }

  // expected-note@+1 {{add '@MainActor' to make instance method 'callH()' part of global actor 'MainActor'}}
  func callH() {
    // expected-error@+1 {{call to main actor-isolated instance method 'h()' in a synchronous nonisolated context}}
    return IsolatedB().h()
  }

  // expected-note@+1 {{add '@MainActor' to make instance method 'callK()' part of global actor 'MainActor'}}
  func callK() {
    // expected-error@+1 {{call to main actor-isolated instance method 'k()' in a synchronous nonisolated context}}
    return IsolatedC().k()
  }

  // expected-note@+1 {{add '@MainActor' to make instance method 'callK2()' part of global actor 'MainActor'}}
  func callK2() {
    // expected-error@+1 {{call to main actor-isolated instance method 'k2()' in a synchronous nonisolated context}}
    return IsolatedCFlipped().k2()
  }
}

@MainActor
struct TestIsolated : NonisolatedWithMembers {
  var x: NonSendable // expected-note {{property declared here}}

  // requirement behaves as if it's explicitly `nonisolated` which gets inferred onto the witness
  func test() {
    _ = x // expected-error {{main actor-isolated property 'x' can not be referenced from a nonisolated context}}
  }
}

@MainActor
protocol Root {
  func testRoot()
}

nonisolated protocol Child : Root {
  func testChild()
}

struct TestDifferentLevels : Child {
  func testRoot() {}
  func testChild() {}
  func testNonWitness() {}
}

nonisolated func testRequirementsOnMultipleNestingLevels(t: TestDifferentLevels) {
  t.testRoot() // okay
  t.testChild() // okay
  t.testNonWitness() // okay
}

// MARK: - Extensions

nonisolated extension GloballyIsolated {
  var x: NonSendable { .init () }
  func implicitlyNonisolated() {}
}

struct C: GloballyIsolated {
  nonisolated func explicitlyNonisolated() {
    let _ = x // okay
    implicitlyNonisolated() // okay
  }
}

// MARK: - Enums

nonisolated enum E: GloballyIsolated {
  func implicitlyNonisolated() {}
  init() {}
}

struct TestEnum {
  nonisolated func call() {
    E().implicitlyNonisolated() // okay
  }
}

// MARK: - Classes

nonisolated class K: GloballyIsolated {
  var x: NonSendable
  init(x: NonSendable) {
    self.x = x // okay
  }
}

@MainActor
protocol GloballyIsolatedWithRequirements {
  var x: NonSendable { get set } // expected-note {{property declared here}}
  func test() // expected-note {{calls to instance method 'test()' from outside of its actor context are implicitly asynchronous}}
}

nonisolated class K2: GloballyIsolatedWithRequirements {
  var x: NonSendable

  func test() {}

  func testNonWitness() {}

  init(x: NonSendable) {
    self.x = x // okay
    test() // okay
    testNonWitness() // okay
  }

  func test<T: GloballyIsolatedWithRequirements>(t: T, s: K2) {
    _ = s.x // okay
    _ = t.x // expected-error {{main actor-isolated property 'x' can not be referenced from a nonisolated context}}

    s.test() // okay
    t.test() // expected-error {{call to main actor-isolated instance method 'test()' in a synchronous nonisolated context}}
  }
}

// MARK: - Storage of non-Sendable

class KlassA {
  nonisolated var test: NonSendable = NonSendable()
}

// MARK: - Restrictions

@MainActor
nonisolated struct Conflict {}
// expected-error@-1 {{struct 'Conflict' has multiple actor-isolation attributes (@MainActor and 'nonisolated')}}

struct B: Sendable {
  // expected-error@+1 {{'nonisolated' can not be applied to variable with non-'Sendable' type 'NonSendable}}
  nonisolated let test: NonSendable
}

final class KlassB: Sendable {
  // expected-note@+2 {{convert 'test' to a 'let' constant or consider declaring it 'nonisolated(unsafe)' if manually managing concurrency safety}}
  // expected-error@+1 {{'nonisolated' cannot be applied to mutable stored properties}}
  nonisolated var test: Int = 1
}

class NotSendable {}

@MainActor
struct UnsafeInitialization {
  nonisolated(unsafe) let ns: NotSendable

  nonisolated init(ns: NotSendable) {
    self.ns = ns // okay
  }
}

// rdar://147965036 - Make sure we don't crash.
func rdar147965036() {
  func test(_: () -> Void) {}
  test { @nonisolated in
    // expected-error@-1 {{'nonisolated' is a declaration modifier, not an attribute}}
    // expected-error@-2 {{'nonisolated' is not supported on a closure}}
  }
}
