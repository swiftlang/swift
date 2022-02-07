// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -swift-version 6 -emit-module -emit-module-path %t/other_global_actor_inference.swiftmodule -module-name other_global_actor_inference -warn-concurrency %S/Inputs/other_global_actor_inference.swift
// RUN: %target-typecheck-verify-swift -swift-version 6 -I %t -disable-availability-checking
// REQUIRES: concurrency
// REQUIRES: asserts
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

  var computedProp: Int { 0 } // expected-note {{property declared here}}

  nonisolated func testErrors() {
    _ = thing // expected-error {{property 'thing' isolated to global actor 'MainActor' can not be referenced from a non-isolated synchronous context}}
    _ = _thing.wrappedValue // expected-error {{property 'wrappedValue' isolated to global actor 'MainActor' can not be referenced from a non-isolated synchronous context}}

    _ = _thing
    _ = _thing.accessCount

    _ = plainStorage

    _ = computedProp // expected-error {{property 'computedProp' isolated to global actor 'MainActor' can not be referenced from a non-isolated synchronous context}}
  }
}

struct HasWrapperOnActor {
  @WrapperOnActor var synced: Int = 0
  // expected-note@-1 2{{property declared here}}

  // expected-note@+1 3{{to make instance method 'testErrors()'}}
  func testErrors() {
    _ = synced // expected-error{{property 'synced' isolated to global actor 'MainActor' can not be referenced from this synchronous context}}
    _ = $synced // expected-error{{property '$synced' isolated to global actor 'SomeGlobalActor' can not be referenced from this synchronous context}}
    _ = _synced
    _ = _synced.wrappedValue // expected-error{{property 'wrappedValue' isolated to global actor 'MainActor' can not be referenced from this synchronous context}}
  }

  @MainActor mutating func testOnMain() {
    _ = synced
    synced = 17
  }

  @WrapperActor var actorSynced: Int = 0

  func testActorSynced() {
    _ = actorSynced
    _ = $actorSynced
    _ = _actorSynced
  }
}

struct Carbon {
  @IntWrapper var atomicWeight: Int // expected-note {{property declared here}}

  nonisolated func getWeight() -> Int {
    return atomicWeight // expected-error {{property 'atomicWeight' isolated to global actor 'MainActor' can not be referenced from a non-isolated synchronous context}}
  }
}
