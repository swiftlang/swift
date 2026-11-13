// RUN: %target-swift-frontend -typecheck -verify -target %target-swift-5.1-abi-triple -swift-version 6 %s

// REQUIRES: concurrency

protocol P: SendableMetatype {
  func f()
}

@preconcurrency
protocol Q: SendableMetatype {
  func f()
}

// expected-error@+1{{cannot form main actor-isolated conformance of 'PSendableSMainActor' to SendableMetatype-inheriting protocol 'P'}}
@MainActor struct PSendableSMainActor: @MainActor P {
  func f() { }
}

// expected-warning@+1{{cannot form main actor-isolated conformance of 'QSendableSMainActor' to SendableMetatype-inheriting protocol 'Q'}}
@MainActor struct QSendableSMainActor: @MainActor Q {
  func f() { }
}

protocol Proto {
  func doThing()
}

actor OtherActor { }

@globalActor
struct SomeGlobalActor {
  static let shared = OtherActor()
}

struct MainType: @MainActor Proto {
  func doThing() { }
}

struct OtherType: @SomeGlobalActor Proto {
  func doThing() { }
}

func acceptTwo<T: Proto, U: Proto>(_ v1: T, _ v2: U) { }

@MainActor func testMultipleIsolatedGenericArgs() {
  acceptTwo(MainType(), MainType()) // okay
  acceptTwo(MainType(), OtherType()) // expected-error{{global actor 'SomeGlobalActor'-isolated conformance of 'OtherType' to 'Proto' cannot be used in main actor-isolated context}}
  acceptTwo(OtherType(), MainType()) // expected-error{{global actor 'SomeGlobalActor'-isolated conformance of 'OtherType' to 'Proto' cannot be used in main actor-isolated context}}
}
