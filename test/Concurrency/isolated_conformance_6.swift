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
