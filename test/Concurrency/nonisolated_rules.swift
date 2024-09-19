// RUN: %target-swift-frontend -disable-availability-checking -swift-version 6 -enable-experimental-feature GlobalActorInferenceCutoff -parse-as-library %s -emit-sil -o /dev/null -verify -strict-concurrency=complete

// REQUIRES: concurrency
// REQUIRES: asserts

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


nonisolated struct NonisolatedStruct: GloballyIsolated {
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

struct A: Refined {
  var x: NonSendable
  init(x: NonSendable) {
    self.x = x // okay
  }
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

// MARK: - Storage of non-Sendable

class KlassA {
  nonisolated var test: NonSendable = NonSendable()
}

// MARK: - Restrictions

@MainActor
nonisolated struct Conflict {}
// expected-error@-1 {{struct 'Conflict' has multiple actor-isolation attributes ('nonisolated' and 'MainActor')}}

struct B: Sendable {
  // expected-error@+1 {{'nonisolated' can not be applied to variable with non-'Sendable' type 'NonSendable}}
  nonisolated let test: NonSendable
}

final class KlassB: Sendable {
  // expected-note@+2 {{convert 'test' to a 'let' constant or consider declaring it 'nonisolated(unsafe)' if manually managing concurrency safety}}
  // expected-error@+1 {{'nonisolated' cannot be applied to mutable stored properties}}
  nonisolated var test: Int = 1
}
