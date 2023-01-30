// RUN: %target-typecheck-verify-swift -target %target-cpu-apple-macosx10.50

// REQUIRES: concurrency
// REQUIRES: OS=macosx

actor SomeActor {}

@globalActor struct AlwaysAvailableGA {
  static let shared = SomeActor()
}

@available(macOS 10.51, *)
@globalActor struct Available10_51GA {
  static let shared = SomeActor()
}

@available(*, unavailable)
@globalActor struct UnavailableGA { // expected-note {{'UnavailableGA' has been explicitly marked unavailable here}}
  static let shared = SomeActor()
}

@AlwaysAvailableGA
struct AlwaysAvailableWithAlwaysAvailableGA {}

@Available10_51GA // expected-error {{'Available10_51GA' is only available in macOS 10.51 or newer}}
struct AlwaysAvailableWithAvailable10_51GA {} // expected-note {{add @available attribute to enclosing struct}}

@available(macOS 10.51, *)
@Available10_51GA
struct Always10_51WithAvailable10_51GA {}

@UnavailableGA // expected-error {{'UnavailableGA' is unavailable}}
struct AlwaysAvailableWithUnavailableGA {}

@available(*, unavailable)
@UnavailableGA
struct UnavailableWithUnavailableGA {}
