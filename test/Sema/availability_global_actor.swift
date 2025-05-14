// RUN: %target-typecheck-verify-swift -target %target-cpu-apple-macosx50

// REQUIRES: concurrency
// REQUIRES: OS=macosx

actor SomeActor {}

@globalActor struct AlwaysAvailableGA {
  static let shared = SomeActor()
}

@available(macOS 51, *)
@globalActor struct Available51GA {
  static let shared = SomeActor()
}

@available(*, unavailable)
@globalActor struct UnavailableGA { // expected-note {{'UnavailableGA' has been explicitly marked unavailable here}}
  static let shared = SomeActor()
}

@AlwaysAvailableGA
struct AlwaysAvailableWithAlwaysAvailableGA {}

@Available51GA // expected-error {{'Available51GA' is only available in macOS 51 or newer}}
struct AlwaysAvailableWithAvailable51GA {} // expected-note {{add '@available' attribute to enclosing struct}}

@available(macOS 51, *)
@Available51GA
struct Always51WithAvailable51GA {}

@UnavailableGA // expected-error {{'UnavailableGA' is unavailable}}
struct AlwaysAvailableWithUnavailableGA {}

@available(*, unavailable)
@UnavailableGA
struct UnavailableWithUnavailableGA {}
