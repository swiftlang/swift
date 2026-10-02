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

// https://github.com/swiftlang/swift/issues/91755
// Error rather than warning in Swift 6 mode.

protocol R {
  func f()
}

@MainActor
class CIsolatedR: @MainActor R {
  func f() { }
}

struct GenericR<T: R>: R {
  func f() { }
}

func testErasureOfSpecializedConformance(_ g: GenericR<CIsolatedR>) {
  _ = g as any R
  // expected-error@-1 {{main actor-isolated conformance of 'CIsolatedR' to 'R' cannot be used in nonisolated context}}
}
