// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -swift-version 6 -emit-module -emit-module-path %t/other_global_actor_inference.swiftmodule -module-name other_global_actor_inference -strict-concurrency=complete %S/Inputs/other_global_actor_inference.swift

// RUN: %target-swift-frontend -swift-version 6 -I %t -disable-availability-checking %s -emit-sil -o /dev/null -verify

// REQUIRES: concurrency

import other_global_actor_inference

actor SomeActor { }

@globalActor
struct SomeGlobalActor {
  static let shared = SomeActor()
}

@globalActor
struct OtherGlobalActor {
  static let shared = SomeActor()
}


// MARK: Property Wrappers

@propertyWrapper
actor WrapperActor<Wrapped: Sendable> {
  var storage: Wrapped

  init(wrappedValue: Wrapped) {
    storage = wrappedValue
  }

  nonisolated var wrappedValue: Wrapped {
    get { }
    set { }
  }

  nonisolated var projectedValue: Wrapped {
    get { }
    set { }
  }
}

@propertyWrapper
@OtherGlobalActor
struct WrapperOnActor<Wrapped: Sendable> {
  private var stored: Wrapped

  nonisolated init(wrappedValue: Wrapped) {
    stored = wrappedValue
  }

  @MainActor var wrappedValue: Wrapped { // expected-note {{property declared here}}
    get { }
    set { }
  }

  @SomeGlobalActor var projectedValue: Wrapped {
    get {  }
    set { }
  }
}

@MainActor
@propertyWrapper
public struct WrapperOnMainActor<Wrapped> {
  // Make sure inference of @MainActor on wrappedValue doesn't crash.

  public var wrappedValue: Wrapped // expected-note {{property declared here}}

  public var accessCount: Int

  nonisolated public init(wrappedValue: Wrapped) {
    self.wrappedValue = wrappedValue
  }
}

struct HasMainActorWrappedProp {
  @WrapperOnMainActor var thing: Int = 1 // expected-note {{property declared here}}

  var plainStorage: Int

  var computedProp: Int { 0 }

  nonisolated func testErrors() {
    _ = thing // expected-error {{main actor-isolated property 'thing' can not be referenced from a nonisolated context}}
    _ = _thing.wrappedValue // expected-error {{main actor-isolated property 'wrappedValue' can not be referenced from a nonisolated context}}

    _ = _thing
    _ = _thing.accessCount

    _ = plainStorage

    _ = computedProp
  }
}

struct HasWrapperOnActor {
  @WrapperOnActor var synced: Int = 0
  // expected-note@-1 2{{property declared here}}

  // expected-note@+1 3{{to make instance method 'testErrors()'}}
  func testErrors() {
    _ = synced // expected-error{{main actor-isolated property 'synced' can not be referenced from a nonisolated context}}
    _ = $synced // expected-error{{global actor 'SomeGlobalActor'-isolated property '$synced' can not be referenced from a nonisolated context}}
    _ = _synced
    _ = _synced.wrappedValue // expected-error{{main actor-isolated property 'wrappedValue' can not be referenced from a nonisolated context}}
  }

  @MainActor mutating func testOnMain() {
    _ = synced
    synced = 17
  }

  @WrapperActor var actorSynced: Int = 0 // expected-error{{'nonisolated' is not supported on properties with property wrappers}}

  func testActorSynced() {
    _ = actorSynced
    _ = $actorSynced
    _ = _actorSynced
  }
}

struct Carbon {
  @IntWrapper var atomicWeight: Int // expected-note {{property declared here}}

  nonisolated func getWeight() -> Int {
    return atomicWeight // expected-error {{main actor-isolated property 'atomicWeight' can not be referenced from a nonisolated context}}
  }
}

@MainActor
protocol InferMainActor {}

@propertyWrapper
@preconcurrency @MainActor
struct Wrapper<T> {
  var wrappedValue: T {
    fatalError()
  }

  init() {}
}

@MainActor
class C {
  nonisolated init() {}
}

struct S: InferMainActor {
  @Wrapper var value: C // okay, 'S' is isolated to 'MainActor'
}

protocol InferMainActorInherited: InferMainActor {
  func f()
  func g()
}

@SomeGlobalActor
protocol InferSomeGlobalActor { }

protocol InferenceConflict: InferMainActorInherited, InferSomeGlobalActor { }

struct S2: InferMainActorInherited {
  func f() { } // okay, 'f' is MainActor isolated, as is the requirement
  @MainActor func g() { } // okay for the same reasons, but more explicitly
}

// expected-error@+2{{conformance of 'S3' to protocol 'InferMainActorInherited' involves isolation mismatches and can cause data races}}
@SomeGlobalActor
struct S3: InferenceConflict {
  // expected-note@-1{{mark all declarations used in the conformance 'nonisolated'}}
  // expected-note@-2{{turn data races into runtime errors with '@preconcurrency'}}
  nonisolated func g() { }
}

extension S3 {

  func f() { }
  // expected-note@-1{{global actor 'SomeGlobalActor'-isolated instance method 'f()' cannot satisfy main actor-isolated requirement}}
}

@MainActor
func onMain() {}

@MainActor
class MainActorSuperclass {}

protocol InferMainFromSuperclass: MainActorSuperclass {
  func f()
}

class C1: MainActorSuperclass, InferMainFromSuperclass {
  func f() {
    onMain() // okay
  }
}

protocol InferenceConflictWithSuperclass: MainActorSuperclass, InferSomeGlobalActor {
  func g()
}

// expected-error@+1{{conformance of 'C2' to protocol 'InferenceConflictWithSuperclass' crosses into main actor-isolated code and can cause data races}}
class C2: MainActorSuperclass, InferenceConflictWithSuperclass {
  //expected-note@-1 {{turn data races into runtime errors with '@preconcurrency'}}
  // expected-note@-2{{mark all declarations used in the conformance 'nonisolated'}}
  // expected-note@-3{{isolate this conformance to the main actor with '@MainActor'}}

  func f() {}

  func g() {}
  // expected-note@-1 {{main actor-isolated instance method 'g()' cannot satisfy nonisolated requirement}}
}


class ConformInExtension {}
extension ConformInExtension: InferMainActor {}

class InheritConformance: ConformInExtension {
  func f() {}
}

func testInheritedMainActorConformance() {
  InheritConformance().f() // okay; this is not MainActor isolated
}
